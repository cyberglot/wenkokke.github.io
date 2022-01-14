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
import Data.Maybe (fromMaybe)
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
  getMetadata <- newCache $ \inputFile -> do
    readYaml' @Metadata inputFile
  let ?defaultMetadata = getMetadata "site.yml"

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
  routes <-
    join
      <$> sequence
        [ [assetSrcDir <//> "*"] |-> assetRouter,
          [styleSrcDir </> "*.scss"] |-> styleRouter,
          [postSrcDir </> "*.md"] |-> postRouter,
          "index.html" |-> outDir </> "index.html",
          "404.html" |-> outDir </> "404.html"
        ]
  want (snd <$> routes)
  let ?routingTable = makeRoutingTable routes

  -- Fix Agda links
  postLinkFixer <- Agda.makeLocalLinkFixer postLibrary
  standardLibraryLinkFixer <- Agda.makeLibraryLinkFixer standardLibrary
  let builtinLinkFixer = Agda.makeBuiltinLinkFixer standardLibrary
  let ?pandocTransform =
        Pandoc.withUrls $ builtinLinkFixer . standardLibraryLinkFixer . postLinkFixer

  -- Assets
  alternatives $ do
    styleRules -- Compile .scss to .css
    assetRules -- Copy assets

  -- Compile posts
  postRules

  -- index.html
  outDir </> "index.html" %> \out -> do
    -- Gather post metadata
    let postSrcs = filter isPostSrc (fst <$> routes)
    need postSrcs
    postsMetadata <- forM postSrcs $ \postSrc -> do
      postOut <- route postSrc
      (postMetadata, postBody) <- readFileWithMetadata' postSrc
      return $
        mconcat
          [ postMetadata,
            constField "url" ("/" <> makeRelative outDir postOut),
            dateFromFileNameField postSrc "date",
            teaserField postBody "teaser"
          ]
    let postsField = constField "post" (reverse postsMetadata)
    -- Compile index page
    src <- routeRev out
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

postSrcDir, postTmpDir, postOutDir :: FilePath
postSrcDir = "posts"
postTmpDir = tmpDir </> "posts"
postOutDir = outDir -- NOTE: cannot rely on 'postOutDir' to test if a FilePath is an output

postRouter :: (?agdaLibraries :: [Agda.Library]) => FilePath -> Rules [FilePath]
postRouter src = do
  PostInfo {..} <- parsePostInfo (makeRelative postSrcDir src)
  let out = postOutDir </> year </> month </> day </> fileName </> "index.html"
  if "lagda" `elem` fileExts
    then do
      tmp <- Agda.markdownOutputPath postTmpDir ?agdaLibraries src
      return [tmp, out]
    else do return [out]

isPostSrc, isPostTmp :: FilePath -> Bool
isPostSrc src = postSrcDir `isPrefixOf` src
isPostTmp tmp = postTmpDir `isPrefixOf` tmp

isPostOut :: (?routingTable :: RoutingTable) => FilePath -> Bool
isPostOut out = not (isPostTmp out) && maybe False isPostSrc (routeRev out)

getReferences :: FilePath -> Action Meta
getReferences src = do
  let bib = replaceExtensions src "bib"
  bibExists <- doesFileExist bib
  if bibExists
    then do
      contents <- readFile' bib
      runPandocIO $ do
        Pandoc meta _ <- readBibTeX def contents
        return meta
    else return mempty

postRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplateFile :: FilePath -> Action Template,
    ?defaultMetadata :: Action Metadata,
    ?agdaLibraries :: [Agda.Library],
    ?pandocTransform :: Pandoc -> Pandoc
  ) =>
  Rules ()
postRules = do
  -- Compile Markdown to HTML and apply templates
  isPostOut ?> \out -> do
    -- Pandoc options
    let readerOpts = def {readerExtensions = markdownDialect}
    let writerOpts = def

    -- Optional bibliography
    bibMeta <- getReferences =<< routeRev out

    src <- routePrev out
    (yamlHeader, markdownBody) <- readFileWithMetadata' src

    -- Render body as Html
    htmlBody <- runPandocIO $ do
      Pandoc docMeta docBlocks <- readMarkdown readerOpts markdownBody
      let doc1 = Pandoc (docMeta <> bibMeta) docBlocks
      doc2 <- processCitations doc1
      let doc3 = ?pandocTransform doc2
      let doc4 = withUrls (relativizeUrl out) doc3
      let doc5 = withUrls (Text.replace "index.html" "") doc4
      writeHtml5String writerOpts doc5

    -- Apply templates
    html <- applyTemplates ["post.html", "default.html"] yamlHeader htmlBody
    writeFile' out html

  -- Compile literate Agda to Markdown & HTML
  isPostTmp ?> \tmp -> do
    liftIO $ putStrLn tmp
    src <- routePrev tmp
    agdaToHtml src

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
        Agda.markdownArgs postTmpDir,
        Agda.libraryArgs ?agdaLibraries,
        [src]
      ]

-- Markdown

markdownDialect :: Extensions
markdownDialect = pandocExtensions
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
        src <- routeRev out
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
    src <- routeRev out
    copyFile' src out