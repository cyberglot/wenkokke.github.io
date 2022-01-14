module Blag.Prelude.FilePath
  ( removeFile_,
    regularFile,
    (~~*?),
    (==*?),
    extensions,
    module Find,
    module FilePath,
  )
where

import Control.Exception (IOException, catch, handle)
import Control.Monad (liftM, when)
import Development.Shake.FilePath as FilePath
import System.Directory (Permissions (..), getPermissions, removeFile, setPermissions)
import System.FilePath.Find as Find
import System.FilePath.GlobPattern (GlobPattern)
import System.IO.Error (isPermissionError)

--------------------------------------------------------------------------------
-- File handling
--
-- Taken from shake General.Extra
--------------------------------------------------------------------------------

removeFile_ :: FilePath -> IO ()
removeFile_ fp =
  removeFile fp `catchIO` \e ->
    when (isPermissionError e) $
      handleIO (\_ -> return ()) $ do
        perms <- getPermissions fp
        setPermissions fp perms {readable = True, searchable = True, writable = True}
        removeFile fp

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = handle

--------------------------------------------------------------------------------
-- Find files
--------------------------------------------------------------------------------

regularFile :: FilterPredicate
regularFile = fileType ==? RegularFile

(~~*?) :: FindClause FilePath -> [GlobPattern] -> FilterPredicate
file ~~*? globPatterns = foldr (\globPattern -> (file ~~? globPattern ||?)) (return False) globPatterns

(==*?) :: FindClause FilePath -> [GlobPattern] -> FilterPredicate
file ==*? globPatterns = foldr (\globPattern -> (file ==? globPattern ||?)) (return False) globPatterns

extensions :: FindClause FilePath
extensions = takeExtensions <$> fileName
