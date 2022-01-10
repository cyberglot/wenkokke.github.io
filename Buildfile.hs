module Main where

import Build.Prelude
import Build.Style.CSS
import Build.Style.Sass

siteDir, cacheDir :: FilePath
siteDir = "_site"
cacheDir = "_cache"

main :: IO ()
main = do
  -- Gather files
  assetSrcFiles <- getDirectoryFilesIO "" [assetSrcDir <//> "*"]
  styleSrcFiles <- getDirectoryFilesIO "" [styleSrcDir </> "*.scss"]

  shakeArgs shakeOptions $ do
    -- Assets
    alternatives $ do
      styleRules -- Compile SCSS to CSS
      want (styleOut <$> styleSrcFiles)
      assetRules -- Copy assets
      want (assetOut <$> assetSrcFiles)

  -- Compile {index,publications,recipes} page to Markdown+HTML
  -- Compile literate Agda to Markdown+HTML
  -- Compile Markdown+HTML to HTML
  -- Apply HTML templates
  return ()

-- Style Files

styleSrcDir, styleOutDir :: FilePath
styleSrcDir = "sass"
styleOutDir = siteDir </> "assets" </> "css"

styleSrc, styleOut :: FilePath -> FilePath
styleSrc file = styleSrcDir </> makeRelative styleOutDir file -<.> "scss"
styleOut file = styleOutDir </> makeRelative styleSrcDir file -<.> "css"

styleRules :: Rules ()
styleRules =
  let sassOptions =
        def
          { sassIncludePaths = Just [styleSrcDir],
            sassImporters = Just [minCssImporter styleSrcDir 1]
          }
   in styleOutDir </> "*.css" %> \out -> do
        css <- compileSassWith sassOptions (styleSrc out)
        cssMin <- minifyCSS css
        writeFile' out cssMin

-- Assets

assetSrcDir, assetOutDir :: FilePath
assetSrcDir = "assets"
assetOutDir = siteDir </> "assets"

assetSrc, assetOut :: FilePath -> FilePath
assetSrc file = assetSrcDir </> makeRelative assetOutDir file
assetOut file = assetOutDir </> makeRelative assetSrcDir file

assetRules :: Rules ()
assetRules =
  assetOutDir <//> "*" %> \out ->
    copyFile' (assetSrc out) out