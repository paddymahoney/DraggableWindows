module Paths_Draggable (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/Draggable-0.0.1/ghc-7.4.2"
datadir    = "/usr/local/share/Draggable-0.0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Draggable_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Draggable_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Draggable_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Draggable_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
