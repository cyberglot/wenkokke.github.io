module Main where

import Blag.Agda qualified as Agda
import Blag.Pandoc as Pandoc
import Blag.PostInfo
import Blag.Prelude
import Blag.Routing
import Blag.Style
import Control.Monad (forM, join)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)

-- cd _site && browser-sync start --server --files "." --no-ui --reload-delay 500 --reload-debounce 500

outDir, tmpDir :: FilePath
outDir = "_site"
tmpDir = "_cache"

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = tmpDir, shakeProgress = progressSimple} $ do
  "clean" ~> do
    removeFilesAfter tmpDir ["//*"]

  "clobber" ~> do
    removeFilesAfter outDir ["//*"]
    removeFilesAfter tmpDir ["//*"]

  -- Metadata
  getSiteMetadata <- newCache $ \() -> do
    readYaml' @Metadata "site.yml"

  getFileMetadata <- newCache $ \src -> do
    siteMetadata <- getSiteMetadata ()
    (yamlHeader, body) <- readFileWithMetadata' src
    lastModifiedMetadata <- lastModifiedField src "modified_date"
    return $
      mconcat
        [ siteMetadata,
          addTitleVariants yamlHeader,
          lastModifiedMetadata,
          constField "source" src,
          constField "body" body
        ]
  let ?getFileMetadata = getFileMetadata

  -- Templates
  getTemplateFile <- newCache $ \inputFile -> do
    let inputPath = "templates" </> inputFile
    need [inputPath]
    compileTemplateFile inputPath
  let ?getTemplateFile = getTemplateFile

  -- Agda Libraries
  standardLibrary <- Agda.getStandardLibrary "agda-stdlib"
  let ?agdaLibraries = [standardLibrary, postLibrary]

  -- Make routing table
  routingTable <- fmap mconcat . sequence $
        [ [assetSrcDir <//> "*"]     |-> assetRouter,
          [styleSrcDir </> "*.scss"] |-> styleRouter,
          [postSrcDir </> "*.md"]    |-> postRouter,
          ["index.html"]             |-> outDir </> "index.html",
          ["404.html"]               |-> outDir </> "404.html"
        ]
  let ?routingTable = routingTable
  want $ outputs routingTable

  -- Fix Agda links
  localLinkFixer <- Agda.makeLocalLinkFixer postLibrary
  standardLibraryLinkFixer <- Agda.makeLibraryLinkFixer standardLibrary
  let builtinLinkFixer = Agda.makeBuiltinLinkFixer standardLibrary
  let ?agdaLinkFixer = builtinLinkFixer . standardLibraryLinkFixer . localLinkFixer

  -- Assets
  alternatives $ do
    styleRules -- Compile .scss to .css
    assetRules -- Copy assets

  -- Compile posts
  postRules

  -- index.html
  outDir </> "index.html" %> \out -> do
    -- Gather post metadata
    let postSrcs = filter isPostSrc (sources ?routingTable)
    need postSrcs
    postsMetadata <- forM postSrcs $ \postSrc -> do
      postOut <- route postSrc
      postMetadata <- readYamlFrontmatter' postSrc
      postBody <- readFile' =<< routeAnchor "html-body" postSrc
      return $
        mconcat
          [ postMetadata,
            constField "url" ("/" <> makeRelative outDir postOut),
            dateFromFileNameField postSrc "date",
            teaserField postBody "teaser"
          ]
    let postsField = constField "post" (reverse postsMetadata)
    -- Compile index page
    src <- routeSrc out
    (yamlHeader, htmlBodyTemplate) <- readFileWithMetadata' src
    htmlBody <- applyAsTemplate htmlBodyTemplate (postsField <> yamlHeader)
    html <- applyTemplate "default.html" yamlHeader htmlBody
    writeFile' out html

  -- 404.html
  outDir </> "404.html" %> \out -> do
    htmlBody <- readFile' "404.html"
    html <- applyTemplate "default.html" mempty htmlBody
    writeFile' out html

  -- Compile {index,publications,recipes} page to Markdown+HTML
  return ()

-- Posts

postSrcDir, postTmp1Dir, postTmp2Dir, postOutDir :: FilePath
postSrcDir = "posts"
postTmp1Dir = tmpDir </> "stage1" </> "posts" -- Render .lagda.md to .md
postTmp2Dir = tmpDir </> "stage2" </> "posts" -- Render .md to .html
postOutDir = outDir -- NOTE: cannot rely on 'postOutDir' to test if a FilePath is an output

postRouter :: (?agdaLibraries :: [Agda.Library]) => FilePath -> Rules [(Maybe Anchor, FilePath)]
postRouter src = do
  let postSrc = makeRelative postSrcDir src
  PostInfo {..} <- parsePostInfo postSrc
  let htmlBody = postTmp2Dir </> postSrc
  let out = postOutDir </> year </> month </> day </> fileName </> "index.html"
  if "lagda" `elem` fileExts
    then do
      highlightAgda <- Agda.markdownOutputPath postTmp1Dir ?agdaLibraries src
      return [(Nothing, highlightAgda), (Just "html-body", htmlBody), (Nothing, out)]
    else do return [(Just "html-body", htmlBody), (Nothing, out)]

isPostSrc, isPostTmp1, isPostTmp2 :: FilePath -> Bool
isPostSrc src = postSrcDir `isPrefixOf` src
isPostTmp1 tmp = postTmp1Dir `isPrefixOf` tmp
isPostTmp2 tmp = postTmp2Dir `isPrefixOf` tmp

isPostOut :: (?routingTable :: RoutingTable) => FilePath -> Bool
isPostOut out = isNothing (routeNext out) && maybe False isPostSrc (routeSrc out)

getReferences :: FilePath -> Action Meta
getReferences src = do
  let bib = replaceExtensions src "bib"
  doesFileExist bib >>= \case
    False -> return mempty
    True -> do
      contents <- readFile' bib
      runPandocIO $ do
        Pandoc meta _ <- readBibTeX def contents
        return meta

postRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?getFileMetadata :: FilePath -> Action Metadata,
    ?agdaLibraries :: [Agda.Library],
    ?agdaLinkFixer :: Url -> Url
  ) =>
  Rules ()
postRules = do
  -- Compile literate Agda to Markdown & HTML
  isPostTmp1 ?> \next -> do
    prev <- routePrev next
    agdaToHtml prev

  -- Compile Markdown to HTML
  isPostTmp2 ?> \next -> do
    (out, prev, src) <-
      (,,) <$> route next <*> routePrev next <*> routeSrc next
    -- Get optional references
    references <- getReferences src
    -- Pandoc options
    let readerOpts = def {readerExtensions = markdownDialect}
    let writerOpts = def
    -- Render Markdown to HTML
    contents <- readFile' prev
    contents <- runPandocIO $ do
      Pandoc meta blocks <- readMarkdown readerOpts contents
      let doc1 = Pandoc (meta <> references) blocks
      doc2 <- processCitations doc1
      let doc3 = withUrls (implicitIndexFile . relativizeUrl out . ?agdaLinkFixer) doc2
      writeHtml5String writerOpts doc3
    writeFile' next contents

  -- Apply templates
  isPostOut ?> \out -> do
    src <- routeSrc out
    metadata <- ?getFileMetadata src
    tmp <- routePrev out
    htmlBody <- readFile' tmp
    html <- applyTemplates ["post.html", "default.html"] metadata htmlBody
    writeFile' out html

-- Agda

postLibrary :: Agda.Library
postLibrary =
  Agda.Library
    { libraryRoot = "",
      includePaths = [postSrcDir],
      canonicalBaseUrl = "https://wen.works/"
    }

agdaToHtml :: (?agdaLibraries :: [Agda.Library]) => FilePath -> Action ()
agdaToHtml src = do
  need [src]
  command_ [] "agda" $
    concat
      [ ["--verbose=0"],
        Agda.markdownArgs postTmp1Dir,
        Agda.libraryArgs ?agdaLibraries,
        [src]
      ]

-- Markdown

markdownDialect :: Extensions
markdownDialect =
  pandocExtensions
    & disableExtension Ext_tex_math_dollars
    & disableExtension Ext_latex_macros

-- Style Files

styleSrcDir, styleOutDir :: FilePath
styleSrcDir = "sass"
styleOutDir = outDir </> "assets" </> "css"

styleRouter :: FilePath -> Rules FilePath
styleRouter src =
  return $
    styleOutDir </> makeRelative styleSrcDir src -<.> "css"

styleRules :: (?routingTable :: RoutingTable) => Rules ()
styleRules =
  let sassOptions =
        def
          { sassIncludePaths = Just [styleSrcDir],
            sassImporters = Just [minCssImporter styleSrcDir 1]
          }
   in styleOutDir </> "*.css" %> \out -> do
        src <- routeSrc out
        css <- compileSassWith sassOptions src
        cssMin <- minifyCSS css
        writeFile' out cssMin

-- Assets

assetSrcDir, assetOutDir :: FilePath
assetSrcDir = "assets"
assetOutDir = outDir </> "assets"

assetRouter :: FilePath -> Rules FilePath
assetRouter src =
  return $
    assetOutDir </> makeRelative assetSrcDir src

assetRules :: (?routingTable :: RoutingTable) => Rules ()
assetRules =
  assetOutDir <//> "*" %> \out -> do
    src <- routeSrc out
    copyFile' src out