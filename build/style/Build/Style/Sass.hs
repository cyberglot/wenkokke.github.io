module Build.Style.Sass
  ( -- * Compile Sass with hsass
    compileSass,
    compileSassWith,
    minCssImporter,
    Sass.SassOptions (..),
  )
where

import Build.Prelude
import Build.Prelude.ByteString qualified as BS (toText)
import Data.Bitraversable (Bitraversable (..))
import Data.Maybe (fromMaybe)
import System.Directory
import System.Directory as IO (doesFileExist, makeAbsolute)
import System.IO (hFlush, stdout)
import Text.Sass as Sass

-- * Sass

minCssImporter :: FilePath -> Double -> SassImporter
minCssImporter includePath priority =
  SassImporter
    { importerPriority = priority,
      importerFunction = \importPath _ -> do
        let minCssPath = includePath </> importPath -<.> "min.css"
        minCssExists <- IO.doesFileExist minCssPath
        if minCssExists
          then do
            minCssSource <- readFile minCssPath
            return [SassImport (Just minCssPath) Nothing (Just minCssSource) Nothing]
          else do
            return []
    }

-- | Compile Sass.
compileSass :: FilePath -> Action Text
compileSass = compileSassWith def

-- | Compile Sass with options.
compileSassWith :: Sass.SassOptions -> FilePath -> Action Text
compileSassWith opts filePath = do
  (css, includes) <- liftIO $ do
    -- Compile @filePath@ from Sass/SCSS to CSS
    resultOrError <- liftIO $ Sass.compileFile filePath opts
    resultOrErrorMsg <- liftIO $ bitraverse Sass.errorMessage return resultOrError
    result <- liftIO (liftEither resultOrErrorMsg)
    -- Extract generated CSS source and included files
    css <- BS.toText (Sass.resultString result)
    includes <- Sass.resultIncludes result
    return (css, includes)

  -- Inform Shake of the dependencies used during compilation
  trackRead includes

  return css

liftEither :: MonadFail m => Either String a -> m a
liftEither (Left e) = fail e
liftEither (Right a) = return a