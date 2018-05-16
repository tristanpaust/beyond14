{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_beyond14 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tristan/Desktop/Spring2018/510/Assignments/Project/beyond14/.cabal-sandbox/bin"
libdir     = "/Users/tristan/Desktop/Spring2018/510/Assignments/Project/beyond14/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2/beyond14-0.1.0.0-HJx5FLZJwRvHXnaBWG6hzo-beyond14"
dynlibdir  = "/Users/tristan/Desktop/Spring2018/510/Assignments/Project/beyond14/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/tristan/Desktop/Spring2018/510/Assignments/Project/beyond14/.cabal-sandbox/share/x86_64-osx-ghc-8.2.2/beyond14-0.1.0.0"
libexecdir = "/Users/tristan/Desktop/Spring2018/510/Assignments/Project/beyond14/.cabal-sandbox/libexec/x86_64-osx-ghc-8.2.2/beyond14-0.1.0.0"
sysconfdir = "/Users/tristan/Desktop/Spring2018/510/Assignments/Project/beyond14/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "beyond14_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "beyond14_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "beyond14_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "beyond14_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "beyond14_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "beyond14_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
