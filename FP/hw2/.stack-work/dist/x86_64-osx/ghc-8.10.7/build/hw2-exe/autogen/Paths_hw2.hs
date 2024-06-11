{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw2 (
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

bindir     = "/Users/mac/JetBrains_Projects/IdeaProjects/hw2/.stack-work/install/x86_64-osx/ea679eb70176de93523b73a506000abb531db92a5eea8556abd1c3dfc4a953c8/8.10.7/bin"
libdir     = "/Users/mac/JetBrains_Projects/IdeaProjects/hw2/.stack-work/install/x86_64-osx/ea679eb70176de93523b73a506000abb531db92a5eea8556abd1c3dfc4a953c8/8.10.7/lib/x86_64-osx-ghc-8.10.7/hw2-0.1.0.0-1Kknv5LgH7q5pDNjtEaPqs-hw2-exe"
dynlibdir  = "/Users/mac/JetBrains_Projects/IdeaProjects/hw2/.stack-work/install/x86_64-osx/ea679eb70176de93523b73a506000abb531db92a5eea8556abd1c3dfc4a953c8/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/mac/JetBrains_Projects/IdeaProjects/hw2/.stack-work/install/x86_64-osx/ea679eb70176de93523b73a506000abb531db92a5eea8556abd1c3dfc4a953c8/8.10.7/share/x86_64-osx-ghc-8.10.7/hw2-0.1.0.0"
libexecdir = "/Users/mac/JetBrains_Projects/IdeaProjects/hw2/.stack-work/install/x86_64-osx/ea679eb70176de93523b73a506000abb531db92a5eea8556abd1c3dfc4a953c8/8.10.7/libexec/x86_64-osx-ghc-8.10.7/hw2-0.1.0.0"
sysconfdir = "/Users/mac/JetBrains_Projects/IdeaProjects/hw2/.stack-work/install/x86_64-osx/ea679eb70176de93523b73a506000abb531db92a5eea8556abd1c3dfc4a953c8/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
