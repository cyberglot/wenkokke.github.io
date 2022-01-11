module Build.Prelude
  ( Text,
    readFile',
    writeFile',
    forEach,
    module Export
  ) where

import Data.Default.Class as Export
import Development.Shake as Export hiding (readFile', writeFile')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Build.Prelude.FilePath as Export
import System.Directory
import Control.Monad (join)

readFile' :: FilePath -> Action Text
readFile' fp = need [fp] >> liftIO (T.readFile fp)

writeFile' :: FilePath -> Text -> Action ()
writeFile' fp content = liftIO $ do
  createDirectoryIfMissing True (takeDirectory fp)
  removeFile_ fp
  T.writeFile fp content

forEach :: (Monad m, Traversable m, Applicative f) => m a -> (a -> f (m b)) -> f (m b)
forEach t f = join <$> traverse f t