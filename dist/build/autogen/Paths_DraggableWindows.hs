module Paths_DraggableWindows (
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

bindir     = "/home/patrick/.cabal/bin"
libdir     = "/home/patrick/.cabal/lib/DraggableWindows-0.0.1/ghc-7.4.2"
datadir    = "/home/patrick/.cabal/share/DraggableWindows-0.0.1"
libexecdir = "/home/patrick/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "DraggableWindows_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DraggableWindows_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "DraggableWindows_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DraggableWindows_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
