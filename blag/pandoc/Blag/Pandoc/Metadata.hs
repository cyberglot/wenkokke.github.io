module Blag.Pandoc.Metadata
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
    lastModifiedISO8601Field,
    postDateField,
    currentDateField,
    constField,
    htmlTeaserField,
    textTeaserField,
    addTitleVariants,
    resolveIncludes,
    module Time,
  )
where

import Blag.PostInfo qualified as PostInfo
import Blag.Prelude (Action, Url, liftIO, need, takeFileName)
import Blag.Prelude.ByteString qualified as BS
import Blag.TagSoup qualified as TagSoup
import Data.Aeson (encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Frontmatter as Frontmatter (IResult (..), parseYamlFrontmatter)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.Time.Format as Time (iso8601DateFormat, rfc822DateFormat)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml qualified as Y
import System.Directory (getModificationTime)
import System.IO.Unsafe (unsafePerformIO)
import Blag.Pandoc.Postprocess (stripTags)

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
  let msg = "Key '" <> Text.unpack key <> "' not found in metadata:\n" <> show metadata
  v <- maybe (fail msg) return $ KeyMap.lookup (Key.fromText key) obj
  case fromJSON v of
    Error msg -> fail msg
    Success a -> return a

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
  show (Metadata obj) = Text.unpack $ unsafePerformIO $ BS.toTextLazy $ encode obj

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

-- | Create a metadata object containing the file modification time.
--
--   Adapted from hakyll's 'Hakyll.Web.Template.Context.modificationTimeField'.
lastModifiedISO8601Field :: FilePath -> Text -> Action Metadata
lastModifiedISO8601Field inputFile key = liftIO $ do
  modificationTime <- getModificationTime inputFile
  return $ constField key (iso8601Show modificationTime)

-- | Create a metadata object containing the current date.
currentDateField :: String -> Text -> Action Metadata
currentDateField fmt key = do
  currentTime <- liftIO getCurrentTime
  let currentTimeString = formatTime defaultTimeLocale fmt currentTime
  return $ constField key currentTimeString

-- | Create a metadata object containing the date inferred from the file path.
postDateField :: MonadFail m => String -> FilePath -> Text -> m Metadata
postDateField fmt inputFile key = do
  let inputFileName = takeFileName inputFile
  postDate <- dateFromPostFileName inputFileName
  let postDateString = formatTime defaultTimeLocale fmt postDate
  return $ constField key postDateString

dateFromPostFileName :: MonadFail m => FilePath -> m UTCTime
dateFromPostFileName postFile = do
  postInfo <- PostInfo.parsePostInfo postFile
  let year = read $ PostInfo.year postInfo
  let month = read $ PostInfo.month postInfo
  let day = read $ PostInfo.day postInfo
  let dateTime = LocalTime (fromGregorian year month day) midday
  return $ localTimeToUTC utc dateTime

-- | Create a metadata object containing the provided value.
constField :: ToJSON a => Text -> a -> Metadata
constField key a = mempty & key .~ a

-- | Create a metadata object containing a teaser constructed from the first argument.
htmlTeaserField :: MonadFail m => FilePath -> Text -> Text -> m Metadata
htmlTeaserField teaserUrl htmlBody key = do
  let htmlBodyWithAbsoluteUrls = htmlTeaserFixUrl teaserUrl htmlBody
  htmlTeaserBody <-  htmlTeaserBody htmlBodyWithAbsoluteUrls
  return $ constField key htmlTeaserBody

htmlTeaserFixUrl :: FilePath -> Text -> Text
htmlTeaserFixUrl teaserUrl = TagSoup.withUrls $ \url ->
  if "#" `Text.isPrefixOf` url
    then Text.pack teaserUrl <> url
    else url

htmlTeaserBody :: MonadFail m => Text -> m Text
htmlTeaserBody body
  | Text.null rest = fail "Delimiter '<!--more-->' not found"
  | otherwise      = return teaser
  where
    (teaser, rest) = Text.breakOn "<!--more-->" body

textTeaserField :: MonadFail m => Text -> Text -> m Metadata
textTeaserField htmlBody key = do
  htmlTeaser <- htmlTeaserBody htmlBody
  let teaser = stripTags htmlTeaser
  return $ constField key teaser


-- | Add running title and subtitle, if title contains a colon.
addTitleVariants :: Metadata -> Metadata
addTitleVariants metadata = case metadata ^. "title" of
  Nothing -> metadata
  Just title ->
    let (titlerunning, subtitle) = Text.breakOn ":" title
     in if Text.null subtitle
          then metadata -- No titlerunning/subtitle distinction
          else
            metadata & "titlerunning" .~ Text.strip titlerunning
              & "subtitle" .~ Text.strip (Text.drop 1 subtitle)

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
