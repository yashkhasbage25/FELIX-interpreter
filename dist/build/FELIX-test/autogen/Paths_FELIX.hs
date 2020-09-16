{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_FELIX (
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

bindir     = "/home/yk/.cabal/bin"
libdir     = "/home/yk/.cabal/lib/x86_64-linux-ghc-8.4.4/FELIX-0.1.0.0-INSe9SylbRV4Fj9nxgulwd-FELIX-test"
dynlibdir  = "/home/yk/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/yk/.cabal/share/x86_64-linux-ghc-8.4.4/FELIX-0.1.0.0"
libexecdir = "/home/yk/.cabal/libexec/x86_64-linux-ghc-8.4.4/FELIX-0.1.0.0"
sysconfdir = "/home/yk/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FELIX_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FELIX_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FELIX_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FELIX_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FELIX_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FELIX_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
