
module Sgf.System.Process
    ( searchExecutable
    , searchPATH
    )
  where

import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Exception
import System.IO.Error
import System.FilePath
import System.Directory

import Sgf.Data.List


-- Search for executable file with specified path and return either its path
-- with slashes (may be relative) or throw exception: if supplied path
-- contains slashes, just check, that file exists and is executable; if
-- supplied path does contain slashes, search for executable file with such
-- name in supplied path list. The search behavior is the same as of execve()
-- function.
searchExecutable :: MonadIO m => FilePath -> [FilePath] -> m FilePath
searchExecutable cmd ps
  | '/' `elem` cmd  =
    liftIO $ catch (searchExecutable (takeFileName cmd) [takeDirectory cmd])
      (\e -> throw $ annotateIOError e "searchExecutable" Nothing (Just cmd))
  | otherwise       = flip evalStateT doesNotExist $ do
    let fs = map (</> cmd) ps
    m <- flip findM fs $ \f -> do
      t <- liftIO $ try (fmap executable (getPermissions f))
      case t of
        Right True  -> return True
        Right False -> put permError >> return False
        -- Ignore only "does not exist" exception and forward others.
        Left e      -> do
          unless (isDoesNotExistError e) (put e)
          return False
    maybe (get >>= throw) return m
  where
    doesNotExist :: IOError
    doesNotExist    = mkIOError doesNotExistErrorType
                        "searchExecutable" Nothing (Just cmd)
    permError :: IOError
    permError       = mkIOError permissionErrorType
                        "searchExecutable" Nothing (Just cmd)

-- Search for executable file (the same way as execve() would do) in PATH
-- modified by a function. The behavior is implemented in `searchExecutable`,
-- except one case: when no function is supplied, assume, that PATH search is
-- turned off and look for file in current directory. Anyway, this function
-- will either return file path with slashes (so execve() would not perform
-- PATH search by itself) or throw exception.
searchPATH :: MonadIO m => FilePath -> Maybe ([FilePath] -> [FilePath])
              -> m FilePath
searchPATH cmd mf   = let f = fromMaybe (const ["."]) mf
                      in  liftIO getSearchPath >>= searchExecutable cmd . f

