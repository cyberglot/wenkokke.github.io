module Main where

import Build.Agda qualified as Agda
import Build.Pandoc as Pandoc
import Build.PostInfo
import Build.Prelude
import Build.Routing
import Build.Style
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)

outDir, tmpDir :: FilePath
outDir = "_site"
tmpDir = "_cache"

main :: IO ()
main = shakeArgs shakeOptions $ do
  -- Metadata
  getMetadata <- newCache $ \inputFile -> do
    readYaml' @Metadata inputFile
  let ?defaultMetadata = getMetadata "site.yml"

  -- Templates
  getTemplate <- newCache $ \inputFile -> do
    let inputPath = "templates" </> inputFile
    need [inputPath]
    compileTemplate inputPath
  let ?getTemplate = getTemplate

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
          "404.html" |-> outDir </> "404.html"
        ]
  want (snd <$> routes)
  let ?routingTable = makeRoutingTable routes

  postLinkFixer <- Agda.makeLocalLinkFixer postLibrary
  standardLibraryLinkFixer <- Agda.makeLibraryLinkFixer standardLibrary
  let builtinLinkFixer = Agda.makeBuiltinLinkFixer standardLibrary
  let ?pandocTransform =
        Pandoc.withUrls $ builtinLinkFixer . standardLibraryLinkFixer . postLinkFixer

  -- Posts
  postRules

  -- Assets
  alternatives $ do
    styleRules -- Compile .scss to .css
    assetRules -- Copy assets

  -- 404.html
  outDir </> "404.html" %> \out ->
    readFile' "404.html"
      >>= applyTemplate "default.html" mempty
      >>= writeFile' out

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
  if bibExists then do
    contents <- readFile' bib
    runPandocIO $ do
      Pandoc meta _ <- readBibTeX def contents
      return meta
  else
    return mempty

postRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplate :: FilePath -> Action Template,
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
        let doc2 = ?pandocTransform doc1
        let doc3 = withUrls (relativizeUrl out) doc2
        writeHtml5String writerOpts doc3

    -- Apply templates
    html <- applyTemplates ["post.html", "default.html"] yamlHeader htmlBody
    writeFile' out html

  -- Compile literate Agda to Markdown & HTML
  isPostTmp ?> \tmp -> do
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

-- markdownToHtml ::
--   ( ?pandocTransform :: Pandoc -> Pandoc
--   ) =>
--   FilePath ->
--   Text ->
--   Action Text
-- markdownToHtml out doc =
--   let readerOpts = def {readerExtensions = markdownDialect}
--       writerOpts = def
--    in runPandocIO $ do
--         doc1 <- readMarkdown readerOpts doc
--         let doc2 = ?pandocTransform doc1
--         let doc3 = withUrls (relativizeUrl out) doc2
--         writeHtml5String writerOpts doc3

markdownDialect :: Extensions
markdownDialect =
  extensionsFromList
    [ Ext_all_symbols_escapable,
      Ext_auto_identifiers,
      Ext_backtick_code_blocks,
      Ext_citations,
      Ext_footnotes,
      Ext_header_attributes,
      Ext_intraword_underscores,
      Ext_markdown_in_html_blocks,
      Ext_shortcut_reference_links,
      Ext_smart,
      Ext_superscript,
      Ext_subscript,
      Ext_task_lists,
      Ext_yaml_metadata_block,
      Ext_raw_html,
      Ext_raw_attribute,
      Ext_fenced_code_blocks,
      Ext_backtick_code_blocks
    ]

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