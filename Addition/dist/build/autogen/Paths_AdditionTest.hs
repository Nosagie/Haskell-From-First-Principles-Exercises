{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_AdditionTest (
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

bindir     = "/Users/nosagie-asaolu/Documents/Projects/Haskell/Haskell From First Principles Exercises/Addition/.cabal-sandbox/bin"
libdir     = "/Users/nosagie-asaolu/Documents/Projects/Haskell/Haskell From First Principles Exercises/Addition/.cabal-sandbox/lib/x86_64-osx-ghc-8.0.2/AdditionTest-0.1.0.0"
dynlibdir  = "/Users/nosagie-asaolu/Documents/Projects/Haskell/Haskell From First Principles Exercises/Addition/.cabal-sandbox/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/nosagie-asaolu/Documents/Projects/Haskell/Haskell From First Principles Exercises/Addition/.cabal-sandbox/share/x86_64-osx-ghc-8.0.2/AdditionTest-0.1.0.0"
libexecdir = "/Users/nosagie-asaolu/Documents/Projects/Haskell/Haskell From First Principles Exercises/Addition/.cabal-sandbox/libexec"
sysconfdir = "/Users/nosagie-asaolu/Documents/Projects/Haskell/Haskell From First Principles Exercises/Addition/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AdditionTest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AdditionTest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AdditionTest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AdditionTest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AdditionTest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AdditionTest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
