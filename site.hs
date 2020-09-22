{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.List (isPrefixOf)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Hakyll
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Regex.TDFA ((=~))
import           Text.CSL.Reference (Reference, RefType(..))
import qualified Text.CSL as CSL
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), (<.>), dropExtension, replaceExtensions)
import           System.Process (system)

--------------------------------------------------------------------------------
-- Site configuration
--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx = constField "version" "1.0.0"
       <> constField "site_title" "All The Language"
       <> constField "site_url" "https://wenkokke.github.io"
       <> constField "email" "wen.kokke@ed.ac.uk"
       <> constField "github_user" "wenkokke"
       <> constField "twitter_user" "wenkokke"
       <> constField "paypal_user" "wenkokke"
       <> defaultContext

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> teaserField "teaser" "content"
       <> siteCtx


--------------------------------------------------------------------------------
-- Pubs configuration
--------------------------------------------------------------------------------
pubSections :: [([RefType], Text)]
pubSections =
  [ ( [ ArticleJournal           ] , "Journal Articles")
  , ( [ Book                     ] , "Books")
  , ( [ Chapter, PaperConference ] , "Conference and Workshop Papers")
  , ( [ Thesis                   ] , "Theses")
  , ( [ Speech                   ] , "Talks" )
  , ( [ NoType                   ] , "Public Houses")
  ]

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

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
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = listField "posts" postCtx (return posts)
                  <> siteCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  -- Compile posts
  match ("posts/*.md" .&&. complement "posts/*.lagda.md") $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls

  -- Compile Literate Agda posts
  match "posts/*.lagda.md" $ do
    route $ gsubRoute "\\.lagda\\.md" (const ".html")
    compile $ agdaCompiler
      >>= renderPandoc
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls

  -- Compile publications
  match "bib/*.bib" $ compile biblioCompiler
  match "csl/*.csl" $ compile cslCompiler
  match "pages/pubs.md" $ do
    route $ setExtension "html"
    compile $ pubsCompiler pubSections
        >>= loadAndApplyTemplate "templates/page.html"    siteCtx
        >>= loadAndApplyTemplate "templates/default.html" siteCtx
        >>= relativizeUrls

  -- Compile 404 page
  match "404.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" siteCtx

  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- Compile Literate Agda posts
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
  let filterRefs :: [RefType] -> [Reference] -> [Reference]
      filterRefs refTypes = filter ((`elem` refTypes) . CSL.refType)

  -- NOTE: pandoc-citeproc renders URL as a link
  let stripLinks :: [Inline] -> [Inline]
      stripLinks = walk $ \case { (Link _ [i] _) -> i ; i -> i }

  let renderSection :: [RefType] -> Text -> [Block]
      renderSection refType sectionTitle =
        [ Header 2 (Text.pack (show refType), [], []) [Str sectionTitle]
        , BulletList [ [Para (stripLinks $ CSL.renderPandoc csl formattedRef)]
                     | let sectionRefs = filterRefs refType refs
                     , let formattedRefs = CSL.processBibliography CSL.procOpts csl sectionRefs
                     , formattedRef <- formattedRefs
                     ]
        ]

  let bibSections :: [Block]
      bibSections = concat (uncurry renderSection <$> sections)

  -- Append bibliography
  let Pandoc meta bs = itemBody docItem
  bibItem <- makeItem $ Pandoc meta (bs ++ bibSections)
  return $ writePandoc bibItem


