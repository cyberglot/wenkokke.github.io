{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor ((<&>))
import           Data.List (isPrefixOf, delete)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Hakyll
import           Text.Pandoc.Options
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Regex.TDFA ((=~))
import           Text.CSL.Reference (Reference, RefType(..))
import           Text.CSL.Style (Agent(..))
import qualified Text.CSL as CSL
import qualified Text.CSL.Pandoc as CSL (processCites)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Environment (getArgs, withArgs)
import           System.FilePath ((</>), (<.>), joinPath, splitDirectories, dropExtension, replaceExtensions)
import           System.Process (system)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx = constField "version" "1.0.0"
       <> constField "site_title" "All The Language"
       <> constField "site_url" "https://wenkokke.github.io"
       <> constField "email" "me@wen.works"
       <> constField "github_user" "wenkokke"
       <> constField "twitter_user" "wenkokke"
       <> constField "paypal_user" "wenkokke"
       <> defaultContext

--------------------------------------------------------------------------------
data Mode =  Production | Draft
  deriving (Eq)

productionConfig :: Configuration
productionConfig = defaultConfiguration
  { destinationDirectory = "_production/site"
  , storeDirectory       = "_production/cache"
  , tmpDirectory         = "_production/cache/tmp"
  }

draftConfig :: Configuration
draftConfig = productionConfig
  { destinationDirectory = "_draft/site"
  , storeDirectory       = "_draft/cache"
  , tmpDirectory         = "_draft/cache/tmp"
  }

config :: Mode -> Configuration
config Production = productionConfig
config Draft = draftConfig

--------------------------------------------------------------------------------

fromGlobs :: [FilePath] -> Pattern
fromGlobs = foldr1 (.||.) . map fromGlob

postDirs :: Mode -> [FilePath]
postDirs mode = "posts" : ["posts" </> "drafts" | mode == Draft]

postPattern :: Mode -> Pattern
postPattern mode = fromGlobs [dir </> "*.md" | dir <- postDirs mode]

postLagdaPattern :: Mode -> Pattern
postLagdaPattern mode = fromGlobs [dir </> "*.lagda.md" | dir <- postDirs mode]

postMarkdownPattern :: Mode -> Pattern
postMarkdownPattern mode = postPattern mode .&&. complement (postLagdaPattern mode)

recipeDirs :: Mode -> [FilePath]
recipeDirs mode = "recipes" : ["recipes" </> "drafts" | mode == Draft]

recipePattern :: Mode -> Pattern
recipePattern mode = fromGlobs [dir </> "*.md" | dir <- recipeDirs mode]

--------------------------------------------------------------------------------
pubSections :: [([RefType], Text)]
pubSections =
  [ ( [ Manuscript               ] , "Drafts")
  , ( [ ArticleJournal           ] , "Journal Articles")
  , ( [ Book                     ] , "Books")
  , ( [ Chapter, PaperConference ] , "Conference and Workshop Papers")
  , ( [ Thesis                   ] , "Theses")
  , ( [ Speech                   ] , "Talks" )
  , ( [ NoType                   ] , "Public Houses")
  ]

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> teaserField "teaser" "content"
       <> siteCtx

recipeCtx :: Context String
recipeCtx = dateField "date" "%B %e, %Y"
         <> teaserField "teaser" "content"
         <> siteCtx

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]

--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "All The Language"
    , feedDescription = "Personal website of Wen Kokke"
    , feedAuthorName  = "Wen Kokke"
    , feedAuthorEmail = "me@wen.works"
    , feedRoot        = "http://wen.works"
    }

--------------------------------------------------------------------------------
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = KaTeX defaultKaTeXUrl
  }

defaultKaTeXUrl :: Text
defaultKaTeXUrl = "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  let mode = if "--drafts" `elem` args then Draft else Production
  let hakyllArgs = delete "--drafts" args
  withArgs hakyllArgs . hakyllWith (config mode) $ do

    -- Copy resources
    match "public/**" $ do
      route idRoute
      compile copyFileCompiler

    -- Compile CSS files
    match "css/*" $ compile compressCssCompiler
    create ["public/css/style.css"] $ do
      route idRoute
      compile $ do
        csses <- loadAll "css/*.css"
        makeItem $ unlines $ map itemBody csses

    -- Compile index page
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll (postPattern mode)
        let indexCtx = listField "posts" postCtx (return posts)
                    <> siteCtx

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    let postBibCompiler :: Item a -> Compiler (Item Biblio)
        postBibCompiler postItem = do
          let postPath = toFilePath (itemIdentifier postItem)
          let bibPath = joinPath
                $ map (\dir -> if dir `elem` postDirs Draft then "bib" else dir)
                $ splitDirectories (replaceExtensions postPath "bib")
          bibPathExists <- unsafeCompiler $ doesFileExist bibPath
          let bibIdentifier = fromFilePath bibPath
          if bibPathExists
            then load bibIdentifier
            else return $ Item bibIdentifier (Biblio [])

    let postCompiler :: Item String -> Compiler (Item String)
        postCompiler item = do
          csl <- load "csl/chicago-author-date.csl"
          bib <- postBibCompiler item
          readPandocWith readerOptions item
            >>= processCites csl bib
            <&> writePandocWith writerOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    -- Compile posts
    match (postMarkdownPattern Draft) $ do
      route $ setExtension "html"
      compile $ getResourceBody >>= postCompiler

    -- Compile Literate Agda posts
    match (postLagdaPattern Draft) $ do
      route $ gsubRoute "\\.lagda\\.md" (const ".html")
      compile $ agdaCompiler >>= postCompiler

    -- Compile publications
    match "bib/*.bib" $ compile biblioCompiler
    match "csl/*.csl" $ compile cslCompiler
    match "pages/pubs.md" $ do
      route $ setExtension "html"
      compile $ pubsCompiler pubSections
          >>= loadAndApplyTemplate "templates/page.html"    siteCtx
          >>= loadAndApplyTemplate "templates/default.html" siteCtx
          >>= relativizeUrls

    -- Compile recipes page
    match "recipes.html" $ do
      route idRoute
      compile $ do
        recipes <- loadAll (recipePattern mode)
        let indexCtx = listField "recipes" recipeCtx (return recipes)
                    <> siteCtx

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    let recipeCompiler :: Item String -> Compiler (Item String)
        recipeCompiler item =
          readPandocWith readerOptions item
            <&> writePandocWith writerOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/recipe.html"  recipeCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    -- Compile recipes
    match (recipePattern Draft) $ do
      route $ setExtension "html"
      compile $ getResourceBody >>= recipeCompiler

    -- Compile 404 page
    match "404.html" $ do
      route idRoute
      compile $ pandocCompilerWith readerOptions writerOptions
        >>= loadAndApplyTemplate "templates/default.html" siteCtx

    -- Render RSS feed
    create ["rss.xml"] $ do
      route idRoute
      compile $ loadAllSnapshots (postPattern mode) "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss feedConfig feedCtx

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- Citations
--------------------------------------------------------------------------------

-- | Process citations in a Pandoc document.
processCites :: Item CSL -> Item Biblio -> Item Pandoc -> Compiler (Item Pandoc)
processCites csl bib item = do
    -- Parse CSL file, if given
    style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    let Biblio refs = itemBody bib
    withItemBody (return . CSL.processCites style refs) item


--------------------------------------------------------------------------------
-- Literate Agda
--------------------------------------------------------------------------------

agdaCompiler :: Compiler (Item String)
agdaCompiler = cached "agda" $ getResourceBody >>= withItemBody renderAgda

renderAgda :: String -> Compiler String
renderAgda code = do
  TmpFile tmpPath <- newTmpFile ".lock"
  let tmpDir     = init (dropExtension tmpPath)
  let moduleName = getAgdaModule code
  let agdaPath   = tmpDir </> moduleName <.> "lagda.md"
  let mdPath     = replaceExtensions agdaPath ".md"

  let fixStdlibUrl :: String -> String
      fixStdlibUrl url
        | moduleName `isPrefixOf` url = url
        | otherwise = "https://agda.github.io/agda-stdlib/" <> url

  unsafeCompiler $ do
    createDirectoryIfMissing True tmpDir
    writeFile agdaPath code
    _ <- system $ unwords
         [ "agda"
         , "-v0"
         , "-i" <> tmpDir
         , "--html"
         , "--html-dir=" <> tmpDir
         , "--html-highlight=code"
         , agdaPath
         ]
    md <- readFile mdPath
    removeDirectory tmpDir
    return $ withUrls fixStdlibUrl md

getAgdaModule :: String -> String
getAgdaModule code = case regexResult of
  (_, _, _, [moduleName]) -> moduleName
  _                       -> "Main"
  where
    moduleRegex = "module ([^ ]*) where" :: String
    regexResult = code =~ moduleRegex :: (String, String, String, [String])


--------------------------------------------------------------------------------
-- Compile bibTeX file to publications list
--------------------------------------------------------------------------------
myAgent :: Agent
myAgent = Agent
  { givenName       = ["Wen"]
  , droppingPart    = mempty
  , nonDroppingPart = mempty
  , familyName      = "Kokke"
  , nameSuffix      = mempty
  , literal         = mempty
  , commaSuffix     = False
  , parseNames      = False
  }

pubsCompiler :: [([RefType], Text)] -> Compiler (Item String)
pubsCompiler sections = do
  -- Read Markdown file
  docItem <- getResourceBody >>= readPandoc

  -- Read CSL file
  cslItem <- load "csl/pubs.csl" :: Compiler (Item CSL)
  csl <- unsafeCompiler $
    CSL.readCSLFile Nothing (toFilePath (itemIdentifier cslItem))

  -- Read Bib file
  Biblio refs <- loadBody "bib/pubs.bib" :: Compiler Biblio

  -- Render the sections
  let filterAgent :: Agent -> Reference -> Reference
      filterAgent a ref = ref { CSL.author = filter (/= a) (CSL.author ref) }

  let filterRefs :: [RefType] -> [Reference] -> [Reference]
      filterRefs refTypes = filter ((`elem` refTypes) . CSL.refType)

  -- NOTE: pandoc-citeproc renders URL as a link, which leads to problems
  --       when you're using CSL literate HTML tags to create links
  let stripLinks :: [Inline] -> [Inline]
      stripLinks = walk $ \case { (Link _ [i] _) -> i ; i -> i }

  let renderSection :: [RefType] -> Text -> [Block]
      renderSection refType sectionTitle =
        [ Header 2 (Text.pack (show refType), [], []) [Str sectionTitle]
        , BulletList [ [Para (stripLinks $ CSL.renderPandoc csl formattedRef)]
                     | let sectionRefs = filterRefs refType refs
                     , let withoutMyAgent = map (filterAgent myAgent) sectionRefs
                     , let formattedRefs = CSL.processBibliography CSL.procOpts csl withoutMyAgent
                     , formattedRef <- formattedRefs
                     ]
        ]

  let bibSections :: [Block]
      bibSections = concat (uncurry renderSection <$> sections)

  -- Append bibliography
  let Pandoc meta bs = itemBody docItem
  bibItem <- makeItem $ Pandoc meta (bs ++ bibSections)
  return $ writePandoc bibItem
