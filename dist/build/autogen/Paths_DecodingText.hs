module Paths_DecodingText (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/arazm/Library/Haskell/bin"
libdir     = "/Users/arazm/Library/Haskell/ghc-7.10.2-x86_64/lib/DecodingText-1.0"
datadir    = "/Users/arazm/Library/Haskell/share/ghc-7.10.2-x86_64/DecodingText-1.0"
libexecdir = "/Users/arazm/Library/Haskell/libexec"
sysconfdir = "/Users/arazm/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DecodingText_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DecodingText_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "DecodingText_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DecodingText_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DecodingText_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
