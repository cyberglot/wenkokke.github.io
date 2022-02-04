module Main where

import Buildfile.Publications
  ( Section (Section),
    makePublicationsPandoc,
  )
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever, join)
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class (Default (def))
import Data.Foldable (Foldable (fold))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.Monoid (Endo (Endo, appEndo))
import Data.MultiMap qualified as MultiMap
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Shoggoth.Agda qualified as Agda
import Shoggoth.PostInfo
import Shoggoth.Prelude
import Shoggoth.Routing
import Shoggoth.Style.CSS
import Shoggoth.Style.Sass
import Shoggoth.Template
import Shoggoth.Template.Pandoc qualified as Pandoc
import Shoggoth.Template.Pandoc.Builder qualified as Builder
import Shoggoth.Template.Pandoc.Citeproc qualified as Citeproc
import Shoggoth.Template.TagSoup qualified as TagSoup
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process (ProcessHandle, cleanupProcess)

outDir, tmpDir :: FilePath
outDir = "_site"
tmpDir = "_cache"

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = tmpDir, shakeProgress = progressSimple} $ do
  --------------------------------------------------------------------------------
  -- Agda libraries

  cachedGetStandardLibrary <- newCache $ \() -> do
    Agda.getStandardLibrary "agda-stdlib"
  let ?getStandardLibrary = cachedGetStandardLibrary

  cachedGetAgdaLibraries <- newCache $ \() -> do
    standardLibrary <- ?getStandardLibrary ()
    let localLibraries = [postLibrary]
    let otherLibraries = []
    return (standardLibrary, localLibraries, otherLibraries)
  let ?getAgdaLibraries = cachedGetAgdaLibraries

  --------------------------------------------------------------------------------
  -- Routing table

  cachedRoutingTable <-
    cacheRoutingTable
      [ ["index.html"] |-> outDir </> "index.html",
        ["rss.xml"] |-> outDir </> "rss.xml",
        [postSrcDir </> "*.md"] |-> postRouter,
        ["pages/recipes.html"] |-> outDir </> "recipes" </> "index.html",
        [recipeSrcDir </> "*.md"] |-> recipeRouter,
        ["pages/pubs.html"] |-> outDir </> "pubs" </> "index.html",
        [assetSrcDir <//> "*"] |-> assetRouter,
        [styleSrcDir </> "*.scss"] |-> styleRouter,
        ["404.html"] |-> outDir </> "404.html"
      ]
  let ?getRoutingTable = cachedRoutingTable

  --------------------------------------------------------------------------------
  -- Cached file, template, and metadata getters

  cachedGetSiteMetadata <- newCache getSiteMetadata
  let ?getSiteMetadata = cachedGetSiteMetadata
  cachedGetFileWithMetadata <- newCache getFileWithMetadata
  let ?getFileWithMetadata = cachedGetFileWithMetadata
  cachedGetPostWithMetadata <- newCache getPostWithMetadata
  let ?getPostWithMetadata = cachedGetPostWithMetadata
  cachedGetPostsMetadata <- newCache getPostsMetadata
  let ?getPostsMetadata = cachedGetPostsMetadata
  cachedGetTemplateFile <- newCache getTemplateFile
  let ?getTemplateFile = getTemplateFile
  cachedGetRecipesMetadata <- newCache getRecipesMetadata
  let ?getRecipesMetadata = cachedGetRecipesMetadata

  --------------------------------------------------------------------------------
  -- Agda link fixers

  cachedGetAgdaLinkFixer <- newCache $ \() -> getAgdaLinkFixer
  let ?getAgdaLinkFixer = cachedGetAgdaLinkFixer

  --------------------------------------------------------------------------------
  -- Phony targets

  "build" ~> do
    need [styleOutDir </> "highlight.css"]
    need . outputs =<< ?getRoutingTable ()

  "clean" ~> do
    removeFilesAfter tmpDir ["//*"]

  "clobber" ~> do
    removeFilesAfter outDir ["//*"]
    removeFilesAfter tmpDir ["//*"]

  --------------------------------------------------------------------------------
  -- File targets

  alternatives $ do
    styleRules -- Style sheets
    assetRules -- Static assets

  -- Publications page
  outDir </> "pubs" </> "index.html" %> \out -> do
    src <- routeSrc out
    (fileMetadata, pubsHtmlTemplate) <- ?getFileWithMetadata src
    myName <- fileMetadata ^. "site.author.name"
    body <- makePublicationsPandoc pubSections (Just myName) outDir out fileMetadata
    body <- pandocToHtml5 body
    let bodyFld = constField "body" body
    applyAsTemplate (bodyFld <> fileMetadata) pubsHtmlTemplate
      >>= applyTemplates ["page.html", "default.html"] fileMetadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

  -- Posts
  outDir </> "index.html" %> \out -> do
    src <- routeSrc out
    postMetadata <- ?getPostsMetadata ()
    (fileMetadata, indexHtmlTemplate) <- ?getFileWithMetadata src
    applyAsTemplate (postMetadata <> fileMetadata) indexHtmlTemplate
      >>= applyTemplate "default.html" fileMetadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

  outDir </> "rss.xml" %> \out -> do
    src <- routeSrc out
    postMetadata <- ?getPostsMetadata ()
    (fileMetadata, rssXmlTemplate) <- ?getFileWithMetadata src
    readFile' src
      >>= applyAsTemplate (fileMetadata <> postMetadata)
      >>= writeFile' out

  postRules

  -- Recipes
  outDir </> "recipes" </> "index.html" %> \out -> do
    src <- routeSrc out
    recipeMetadata <- ?getRecipesMetadata ()
    (fileMetadata, recipesHtmlTemplate) <- ?getFileWithMetadata src
    applyAsTemplate (recipeMetadata <> fileMetadata) recipesHtmlTemplate
      >>= applyTemplate "default.html" fileMetadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

  recipeRules

  -- 404.html
  outDir </> "404.html" %> \out -> do
    metadata <- ?getSiteMetadata ()
    readFile' "404.html"
      >>= applyTemplate "default.html" metadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Publications

pubSections :: [Section]
pubSections =
  [ Section ["manuscript"] "Drafts",
    Section ["article-journal"] "Journal Articles",
    Section ["book"] "Books",
    Section ["chapter", "paper-conference"] "Conference and Workshop Papers",
    Section ["thesis"] "Theses",
    Section ["speech"] "Talks",
    Section ["notype"] "Public Houses"
  ]

--------------------------------------------------------------------------------
-- Posts

postSrcDir, postTmp1Dir, postTmp2Dir, postOutDir :: FilePath
postSrcDir = "posts"
postTmp1Dir = tmpDir </> "stage1" </> "posts" -- Render .lagda.md to .md
postTmp2Dir = tmpDir </> "stage2" </> "posts" -- Render .md to .html
postOutDir = outDir -- NOTE: cannot rely on 'postOutDir' to test if a FilePath is an output

postRouter ::
  ( ?getAgdaLibraries :: () -> Action (Agda.Library, [Agda.Library], [Agda.Library])
  ) =>
  FilePath ->
  Action [(Maybe Anchor, FilePath)]
postRouter src = do
  let postSrc = makeRelative postSrcDir src
  PostInfo {..} <- parsePostSource postSrc
  let htmlBody = postTmp2Dir </> postSrc
  let out = postOutDir </> year </> month </> day </> fileName </> "index.html"
  if Agda.isAgdaFile src
    then do
      (standardLibrary, localLibraries, otherLibraries) <- ?getAgdaLibraries ()
      let agdaLibraries = standardLibrary : localLibraries <> otherLibraries
      highlightAgda <- Agda.htmlOutputPath postTmp1Dir agdaLibraries src
      return [(Nothing, highlightAgda), (Just "html-body", htmlBody), (Nothing, out)]
    else do return [(Just "html-body", htmlBody), (Nothing, out)]

isPostSrc, isPostTmp1, isPostTmp2, isPostOut :: FilePath -> Bool
isPostSrc src = postSrcDir `isPrefixOf` src
isPostTmp1 tmp = postTmp1Dir `isPrefixOf` tmp
isPostTmp2 tmp = postTmp2Dir `isPrefixOf` tmp
isPostOut out = isJust (parsePostOutput out)

postRules ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getPostWithMetadata :: FilePath -> Action (Metadata, Text),
    ?getAgdaLibraries :: () -> Action (Agda.Library, [Agda.Library], [Agda.Library]),
    ?getAgdaLinkFixer :: () -> Action (Url -> Url)
  ) =>
  Rules ()
postRules = do
  -- Compile literate Agda to Markdown & HTML
  isPostTmp1 ?> \next -> do
    prev <- routePrev next
    (standardLibrary, localLibraries, otherLibraries) <- ?getAgdaLibraries ()
    let agdaLibraries = standardLibrary : localLibraries <> otherLibraries
    Agda.compileTo Agda.Html agdaLibraries postTmp1Dir prev

  -- Compile Markdown to HTML
  isPostTmp2 ?> \next -> do
    (out, prev, src) <- (,,) <$> route next <*> routePrev next <*> routeSrc next
    agdaLinkFixer <- ?getAgdaLinkFixer ()
    let maybeAgdaLinkFixer = if Agda.isAgdaFile src then Just (Pandoc.withUrls agdaLinkFixer) else Nothing
    readFile' prev
      >>= markdownToPandoc
      >>= processCitations
      <&> walk (shiftHeadersBy 2)
      <&> fromMaybe id maybeAgdaLinkFixer
      >>= pandocToHtml5 -- postprocessHtml5 in next rule
      >>= writeFile' next

  -- Apply templates
  isPostOut ?> \out -> do
    (prev, src) <- (,) <$> routePrev out <*> routeSrc out
    metadata <- fst <$> ?getPostWithMetadata src
    readFile' prev
      >>= applyTemplates ["post.html", "default.html"] metadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Recipes

recipeSrcDir, recipeOutDir :: FilePath
recipeSrcDir = "recipes"
recipeOutDir = outDir </> "recipes"

isRecipeSrc :: FilePath -> Bool
isRecipeSrc src = recipeSrcDir `isPrefixOf` src

recipeRouter :: FilePath -> FilePath
recipeRouter src =
  recipeOutDir </> dropExtension (makeRelative recipeSrcDir src) </> "index.html"

recipeRules ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  Rules ()
recipeRules = do
  recipeOutDir <//> "*" %> \out -> do
    src <- routeSrc out
    metadata <- fst <$> ?getFileWithMetadata src
    readFile' src
      >>= markdownToPandoc
      >>= processCitations
      <&> walk (shiftHeadersBy 2)
      >>= pandocToHtml5
      >>= applyTemplates ["recipe.html", "default.html"] metadata
      <&> postprocessHtml5 outDir out
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Agda

postLibrary :: Agda.Library
postLibrary =
  Agda.Library
    { libraryRoot = "",
      includePaths = [postSrcDir],
      canonicalBaseUrl = "https://wen.works/"
    }

getAgdaLinkFixer ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getAgdaLibraries :: () -> Action (Agda.Library, [Agda.Library], [Agda.Library])
  ) =>
  Action (Url -> Url)
getAgdaLinkFixer = do
  (standardLibrary, localLibraries, otherLibraries) <- ?getAgdaLibraries ()
  let builtinLinkFixer = Agda.makeBuiltinLinkFixer standardLibrary
  standardLibraryLinkFixers <- Agda.makeLibraryLinkFixer standardLibrary
  localLinkFixers <- traverse Agda.makeLocalLinkFixer localLibraries
  otherLinkFixers <- traverse Agda.makeLibraryLinkFixer otherLibraries
  let linkFixers =
        [ [builtinLinkFixer, standardLibraryLinkFixers],
          otherLinkFixers,
          localLinkFixers
        ]
  return . appEndo . mconcat . fmap Endo . concat $ linkFixers

--------------------------------------------------------------------------------
-- Style Sheets

styleSrcDir, styleOutDir :: FilePath
styleSrcDir = "sass"
styleOutDir = outDir </> "assets" </> "css"

styleRouter :: FilePath -> FilePath
styleRouter src = styleOutDir </> makeRelative styleSrcDir src -<.> "css"

sassOptions :: SassOptions
sassOptions =
  def
    { sassIncludePaths = Just [styleSrcDir],
      sassImporters = Just [minCssImporter styleSrcDir 1]
    }

styleRules :: (?getRoutingTable :: () -> Action RoutingTable) => Rules ()
styleRules = alternatives $ do
  styleOutDir </> "style.css" %> \out -> do
    src <- routeSrc out
    compileSassWith sassOptions src
      >>= minifyCSS
      >>= writeFile' out

  styleOutDir </> "highlight.css" %> \out -> do
    let css = Text.pack $ Pandoc.styleToCss highlightStyle
    writeFile' out =<< minifyCSS css

  styleOutDir </> "*.css" %> \out -> do
    src <- routeSrc out
    readFile' src
      >>= minifyCSS
      >>= writeFile' out

--------------------------------------------------------------------------------
-- Assets

assetSrcDir, assetOutDir :: FilePath
assetSrcDir = "assets"
assetOutDir = outDir </> "assets"

assetRouter :: FilePath -> FilePath
assetRouter src = assetOutDir </> makeRelative assetSrcDir src

assetRules :: (?getRoutingTable :: () -> Action RoutingTable) => Rules ()
assetRules =
  assetOutDir <//> "*" %> \out -> do
    src <- routeSrc out
    copyFile' src out

--------------------------------------------------------------------------------
-- Markdown to HTML compilation

shiftHeadersBy :: Int -> Block -> Block
shiftHeadersBy n (Header l attr body) = Header (l + n) attr body
shiftHeadersBy n x = x

highlightStyle :: Pandoc.HighlightStyle
highlightStyle = Pandoc.pygments

readerOpts :: ReaderOptions
readerOpts =
  def
    { readerExtensions = markdownDialect
    }

writerOpts :: WriterOptions
writerOpts =
  def
    { writerHTMLMathMethod = KaTeX "",
      writerEmailObfuscation = JavascriptObfuscation,
      writerHighlightStyle = Just highlightStyle
    }

markdownToPandoc :: Text -> Action Pandoc
markdownToPandoc =
  Pandoc.runPandoc . Pandoc.readMarkdown readerOpts

pandocToHtml5 :: Pandoc -> Action Text
pandocToHtml5 =
  Pandoc.runPandoc . Pandoc.writeHtml5String writerOpts

processCitations :: Pandoc -> Action Pandoc
processCitations =
  Pandoc.runPandoc . Citeproc.processCitations

postprocessHtml5 :: FilePath -> FilePath -> Text -> Text
postprocessHtml5 outDir out html5 =
  html5
    & TagSoup.withUrls (implicitIndexFile . relativizeUrl outDir out)
    & TagSoup.addDefaultTableHeaderScope "col"
    & Pandoc.postprocessHtml5

markdownDialect :: Extensions
markdownDialect = Pandoc.pandocExtensions

--------------------------------------------------------------------------------
-- Metadata

-- | Get the default metadata for the website.
getSiteMetadata :: () -> Action Metadata
getSiteMetadata () = do
  siteMetadata <- readYaml' "site.yml"
  buildDateFld <- currentDateField rfc822DateFormat "build_date"
  let metadata = mconcat [siteMetadata, buildDateFld]
  return $ constField "site" metadata

-- | Get a file body and its metadata, including derived metadata.
--
--   This function adds the following metadata fields,
--   in addition to the metadata added by 'getSiteMetadata':
--
--     - @url@: The URL to the output file.
--     - @body@: The body of the source file.
--     - @source@: The path to the source file.
--     - @modified_date@: The date at which the file was last modified, in the ISO8601 format.
--     - @build_date@: The date at which the is website was last built, in the RFC822 format.
--     - @highlight-css@: The CSS for syntax highlighting, if the original file specifies 'highlight'.
getFileWithMetadata ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getSiteMetadata :: () -> Action Metadata
  ) =>
  FilePath ->
  Action (Metadata, Text)
getFileWithMetadata src = do
  out <- route src
  let url = "/" <> makeRelative outDir out
  siteMetadata <- ?getSiteMetadata ()
  (fileMetadata, body) <- readFileWithMetadata' src
  let urlFld = constField "url" url
  let bodyFld = constField "body" body
  let sourceFld = constField "source" src
  modifiedDateFld <- lastModifiedISO8601Field src "modified_date"
  let metadata = mconcat [siteMetadata, fileMetadata, urlFld, bodyFld, sourceFld, modifiedDateFld]
  return (metadata, body)

-- | Get a metadata object representing all posts.
--
--   This function adds the following metadata fields for each post,
--   in addition to the metadata added by 'getFileWithMetadata':
--
--   - @date@: The date of the post, in a human readable format.
--   - @date_rfc822@: The date of the post, in the RFC822 date format.
getPostWithMetadata ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  FilePath ->
  Action (Metadata, Text)
getPostWithMetadata src = do
  (fileMetadata, body) <- ?getFileWithMetadata src
  dateFld <- postDateField "%a %-d %b, %Y" src "date"
  dateRfc822Fld <- postDateField rfc822DateFormat src "date_rfc822"
  let metadata = mconcat [fileMetadata, dateFld, dateRfc822Fld]
  return (metadata, body)

-- | Get a metadata object representing all posts.
--
--   This function adds the following metadata fields for each post,
--   in addition to the metadata added by 'getFileWithMetadata':
--
--   - @body_html@: The rendered HTML body of the source file.
--   - @teaser_html@: The rendered HTML teaser for the post.
--   - @teaser_plain@: The plain text teaser for the post.
getPostsMetadata ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getPostWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  () ->
  Action Metadata
getPostsMetadata () = do
  -- Get posts from routing table
  routingTable <- ?getRoutingTable ()
  let postSrcs = filter isPostSrc (sources routingTable)
  -- Gather metadata for each post
  postsMetadata <- forM postSrcs $ \src -> do
    -- Get output file for URL and html-body anchor for teaser
    (out, ankHtmlBody) <- (,) <$> route src <*> routeAnchor "html-body" src
    let url = "/" <> makeRelative outDir out
    postMetadata <- fst <$> ?getPostWithMetadata src
    bodyHtml <- readFile' ankHtmlBody
    let bodyHtmlFld = constField "body_html" bodyHtml
    teaserHtmlFld <- htmlTeaserField url bodyHtml "teaser_html"
    teaserPlainFld <- textTeaserField bodyHtml "teaser_plain"
    return $ mconcat [postMetadata, bodyHtmlFld, teaserHtmlFld, teaserPlainFld]
  return $ constField "post" (reverse postsMetadata)

-- | Get a metadata object representing all recipes.
getRecipesMetadata ::
  ( ?getRoutingTable :: () -> Action RoutingTable,
    ?getFileWithMetadata :: FilePath -> Action (Metadata, Text)
  ) =>
  () ->
  Action Metadata
getRecipesMetadata () = do
  -- Get recipes from routing table
  routingTable <- ?getRoutingTable ()
  let recipeSrcs = filter isRecipeSrc (sources routingTable)
  -- Gather metadata for each recipe
  recipesMetadata <- forM recipeSrcs $ \src -> do
    -- Get output file for URL and html-body anchor for teaser
    out <- route src
    let url = "/" <> makeRelative outDir out
    fst <$> ?getFileWithMetadata src
  return $ constField "recipe" recipesMetadata

-- | Get a template from the @templates/@ directory.
getTemplateFile :: FilePath -> Action Template
getTemplateFile inputFile = do
  let inputPath = "templates" </> inputFile
  need [inputPath]
  compileTemplateFile inputPath
