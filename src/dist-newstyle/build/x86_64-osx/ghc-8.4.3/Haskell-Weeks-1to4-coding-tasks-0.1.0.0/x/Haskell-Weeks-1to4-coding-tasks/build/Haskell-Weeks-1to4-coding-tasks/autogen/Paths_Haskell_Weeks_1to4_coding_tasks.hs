{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Haskell_Weeks_1to4_coding_tasks (
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

bindir     = "/Users/tejas/.cabal/bin"
libdir     = "/Users/tejas/.cabal/lib/x86_64-osx-ghc-8.4.3/Haskell-Weeks-1to4-coding-tasks-0.1.0.0-inplace-Haskell-Weeks-1to4-coding-tasks"
dynlibdir  = "/Users/tejas/.cabal/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/tejas/.cabal/share/x86_64-osx-ghc-8.4.3/Haskell-Weeks-1to4-coding-tasks-0.1.0.0"
libexecdir = "/Users/tejas/.cabal/libexec/x86_64-osx-ghc-8.4.3/Haskell-Weeks-1to4-coding-tasks-0.1.0.0"
sysconfdir = "/Users/tejas/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskell_Weeks_1to4_coding_tasks_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskell_Weeks_1to4_coding_tasks_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Haskell_Weeks_1to4_coding_tasks_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Haskell_Weeks_1to4_coding_tasks_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskell_Weeks_1to4_coding_tasks_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskell_Weeks_1to4_coding_tasks_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
