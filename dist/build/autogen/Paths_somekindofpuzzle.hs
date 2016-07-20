module Paths_somekindofpuzzle (
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
version = Version [0,100000000000000,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\x432ph\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\x432ph\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\somekindofpuzzle-0.100000000000000.0.0-3QqwRoS4k9oHkRYGMos7xm"
datadir    = "C:\\Users\\x432ph\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\somekindofpuzzle-0.100000000000000.0.0"
libexecdir = "C:\\Users\\x432ph\\AppData\\Roaming\\cabal\\somekindofpuzzle-0.100000000000000.0.0-3QqwRoS4k9oHkRYGMos7xm"
sysconfdir = "C:\\Users\\x432ph\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "somekindofpuzzle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "somekindofpuzzle_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "somekindofpuzzle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "somekindofpuzzle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "somekindofpuzzle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
