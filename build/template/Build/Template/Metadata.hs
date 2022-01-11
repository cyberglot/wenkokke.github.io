module Build.Template.Metadata
  ( Metadata (..),
    readYaml',
    readYaml,
    writeYaml',
    writeYaml,
    readYamlFrontmatter',
    readYamlFrontmatter,
    readFileWithMetadata',
    readFileWithMetadata,
    (^.),
    (.~),
    (&),
    lastModifiedField,
    dateFromFileNameField,
    currentDateField,
    constField,
    teaserField,
    addTitleVariants,
    resolveIncludes,
  )
where

import Build.Prelude (Action, liftIO, need, takeFileName)
import Build.Prelude.ByteString qualified as BS
import Data.Aeson (encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    Object,
    Result (..),
    ToJSON (toJSON),
    Value (..),
    fromJSON,
    withObject,
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Frontmatter as Frontmatter (IResult (..), parseYamlFrontmatter)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( FormatTime,
    defaultTimeLocale,
    formatTime,
    fromGregorian,
    getCurrentTime,
  )
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml qualified as Y
import System.Directory (getModificationTime)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

newtype Metadata = Metadata Object

infixr 4 .~

-- | Set a value on a metadata object.
(.~) :: ToJSON a => Text -> a -> Metadata -> Metadata
(key .~ value) (Metadata obj) = Metadata $ KeyMap.insert (Key.fromText key) (toJSON value) obj

infixl 8 ^.

-- | Get a value from a metadata object.
(^.) :: (MonadFail m, FromJSON a) => Metadata -> Text -> m a
metadata@(Metadata obj) ^. key = do
  let msg = "Key '" <> T.unpack key <> "' not found in metadata:\n" <> show metadata
  v <- liftMaybe msg $ KeyMap.lookup (Key.fromText key) obj
  liftResult $ fromJSON v

liftMaybe :: MonadFail m => String -> Maybe a -> m a
liftMaybe errorMessage Nothing = fail errorMessage
liftMaybe _ (Just a) = return a

liftResult :: MonadFail m => Result a -> m a
liftResult (Error msg) = fail msg
liftResult (Success a) = return a

--------------------------------------------------------------------------------
-- Reading and writing
--------------------------------------------------------------------------------

-- | Read a YAML file as a Shake action.
readYaml' :: FromJSON a => FilePath -> Action a
readYaml' inputFile = do
  need [inputFile]
  liftIO $ readYaml inputFile

-- | Read a YAML file.
readYaml :: FromJSON a => FilePath -> IO a
readYaml = Y.decodeFileThrow

-- | Read the YAML frontmatter from a file as a Shake action.
readYamlFrontmatter' :: FromJSON a => FilePath -> Action a
readYamlFrontmatter' inputFile = do
  need [inputFile]
  liftIO $ readYamlFrontmatter inputFile

-- | Read the YAML frontmatter from a file.
readYamlFrontmatter :: FromJSON a => FilePath -> IO a
readYamlFrontmatter inputFile =
  fst <$> readFileWithMetadata inputFile

-- | Read a file with its YAML frontmatter and return both as a Shake action.
readFileWithMetadata' :: FromJSON a => FilePath -> Action (a, Text)
readFileWithMetadata' inputFile = do
  need [inputFile]
  liftIO $ readFileWithMetadata inputFile

-- | Read a file with its YAML frontmatter and return both.
readFileWithMetadata :: FromJSON a => FilePath -> IO (a, Text)
readFileWithMetadata inputFile = do
  contents <- B.readFile inputFile
  withResult (parseYamlFrontmatter contents)
  where
    withResult :: (FromJSON a) => IResult ByteString a -> IO (a, Text)
    withResult (Done body metadata) = do tbody <- BS.toText body; return (metadata, tbody)
    withResult (Fail _ _ message) = fail message
    withResult (Partial k) = withResult (k "")

writeYaml' :: ToJSON a => FilePath -> a -> Action ()
writeYaml' outputFile a = liftIO $ Y.encodeFile outputFile a

writeYaml :: ToJSON a => FilePath -> a -> IO ()
writeYaml = Y.encodeFile

-- * Instances

instance Show Metadata where
  show (Metadata obj) = T.unpack $ unsafePerformIO $ BS.toTextLazy $ encode obj

instance ToJSON Metadata where
  toJSON (Metadata obj) = Object obj

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v -> return (Metadata v)

instance Semigroup Metadata where
  (Metadata m1) <> (Metadata m2) = Metadata (KeyMap.union m1 m2)

instance Monoid Metadata where
  mempty = Metadata KeyMap.empty

--------------------------------------------------------------------------------
-- Metadata fields
--------------------------------------------------------------------------------

prettyDate :: FormatTime t => t -> String
prettyDate = formatTime defaultTimeLocale "%a %-d %b, %Y"

-- | Create a metadata object containing the file modification time.
--
--   Adapted from hakyll's 'Hakyll.Web.Template.Context.modificationTimeField'.
lastModifiedField :: FilePath -> Text -> Action Metadata
lastModifiedField inputFile key = liftIO $ do
  modificationTime <- getModificationTime inputFile
  return $ constField key (iso8601Show modificationTime)

-- | Create a metadata object containing the current date.
currentDateField :: Text -> Action Metadata
currentDateField key = liftIO $ do
  constField key . prettyDate <$> getCurrentTime

-- | Create a metadata object containing the date inferred from the file path.
dateFromFileNameField :: FilePath -> Text -> Metadata
dateFromFileNameField inputFile key
  | length chunks >= 3 = constField key (prettyDate date)
  | otherwise = mempty
  where
    inputFileName = takeFileName inputFile
    chunks = map T.unpack $ T.splitOn "-" $ T.pack inputFileName
    ~(y : m : d : _) = map read chunks
    date = fromGregorian y (fromInteger m) (fromInteger d)

-- | Create a metadata object containing the provided value.
constField :: ToJSON a => Text -> a -> Metadata
constField key a = mempty & key .~ a

-- | Create a metadata object containing a teaser constructed from the first argument.
teaserField :: Text -> Text -> Metadata
teaserField body key
  | T.null rest = mempty
  | otherwise = constField key teaserBody
  where
    (teaserBody, rest) = T.breakOn "<!--more-->" body

-- | Add running title and subtitle, if title contains a colon.
addTitleVariants :: Metadata -> Metadata
addTitleVariants metadata = case metadata ^. "title" of
  Nothing -> metadata
  Just title ->
    let (titlerunning, subtitle) = T.breakOn ":" title
     in if T.null subtitle
          then metadata -- No titlerunning/subtitle distinction
          else
            metadata & "titlerunning" .~ T.strip titlerunning
              & "subtitle" .~ T.strip (T.drop 1 subtitle)

-- | Resolve 'include' fields by including metadata from files.
resolveIncludes :: (FilePath -> Action Metadata) -> Metadata -> Action Metadata
resolveIncludes reader (Metadata object) = fromValue <$> walkValue (Object object)
  where
    fromValue :: Value -> Metadata
    fromValue (Object object) = Metadata object
    fromValue v = error $ "Unexpected value " <> show v

    walkValue :: Value -> Action Value
    walkValue (Object object) = do
      object' <- resolveInclude object
      Object <$> traverse walkValue object'
    walkValue (Array array) = Array <$> traverse walkValue array
    walkValue value = return value

    resolveInclude :: Object -> Action Object
    resolveInclude object =
      case Metadata object ^. "include" of
        Nothing -> return object
        Just filePath -> do
          Metadata includedObject <- reader filePath
          return (object <> includedObject)
