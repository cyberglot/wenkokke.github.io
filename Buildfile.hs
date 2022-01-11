module Main where

import Build.PostInfo
import Build.Prelude
import Build.Routing
import Build.Style
import Build.Template
import Control.Monad (join)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

outDir, tmpDir :: FilePath
outDir = "_site"
tmpDir = "_cache"

main :: IO ()
main = shakeArgs shakeOptions $ do
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
  -- Compile literate Agda to Markdown+HTML
  -- Compile Markdown+HTML to HTML
  -- Apply HTML templates
  return ()

-- Posts

postSrcDir, postTmpDir, postOutDir :: FilePath
postSrcDir = "posts"
postTmpDir = tmpDir </> "posts"
postOutDir = outDir -- NOTE: cannot rely on 'postOutDir' to test if a FilePath is an output

postRouter :: FilePath -> Rules [FilePath]
postRouter src = do
  PostInfo {..} <- parsePostInfo (makeRelative postSrcDir src)
  let tmp = postTmpDir </> makeRelative postSrcDir (replaceExtensions src "md")
  let out = postOutDir </> year </> month </> day </> fileName </> "index.html"
  return $ if "lagda" `elem` fileExts then [tmp, out] else [out]

isPostSrc, isPostTmp :: FilePath -> Bool
isPostSrc src = postSrcDir `isPrefixOf` src
isPostTmp tmp = postTmpDir `isPrefixOf` tmp

isPostOut :: (?routingTable :: RoutingTable) => FilePath -> Bool
isPostOut out = not (isPostTmp out) && maybe False isPostSrc (routeRev out)

postRules ::
  ( ?routingTable :: RoutingTable,
    ?getTemplate :: FilePath -> Action Template,
    ?defaultMetadata :: Action Metadata
  ) =>
  Rules ()
postRules = do
  -- Compile Markdown to HTML and apply templates
  isPostOut ?> \out -> do
    src <- routePrev out
    (meta, markdownBody) <- readFileWithMetadata' src
    htmlBody <- markdownToHtml markdownBody
    htmlDoc <- applyTemplates ["post.html", "default.html"] meta htmlBody
    writeFile' out htmlDoc

  -- Compile literate Agda to Markdown & HTML
  isPostTmp ?> \tmp -> do
    src <- routePrev tmp
    _

-- Markdown

markdownToHtml :: Text -> Action Text
markdownToHtml src =
  let readerOpts = def {readerExtensions = markdownDialect}
      writerOpts = def
   in runPandocIO $ do
        ast <- readMarkdown readerOpts src
        writeHtml5String writerOpts ast

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