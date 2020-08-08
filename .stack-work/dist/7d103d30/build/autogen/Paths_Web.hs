{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Web (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Haskell 8.6.5\\Web\\.stack-work\\install\\a1d9d7bd\\bin"
libdir     = "D:\\Haskell 8.6.5\\Web\\.stack-work\\install\\a1d9d7bd\\lib\\x86_64-windows-ghc-8.4.4\\Web-0.0.0-EfJhU7jVSxEEy0b1vmwAjc"
dynlibdir  = "D:\\Haskell 8.6.5\\Web\\.stack-work\\install\\a1d9d7bd\\lib\\x86_64-windows-ghc-8.4.4"
datadir    = "D:\\Haskell 8.6.5\\Web\\.stack-work\\install\\a1d9d7bd\\share\\x86_64-windows-ghc-8.4.4\\Web-0.0.0"
libexecdir = "D:\\Haskell 8.6.5\\Web\\.stack-work\\install\\a1d9d7bd\\libexec\\x86_64-windows-ghc-8.4.4\\Web-0.0.0"
sysconfdir = "D:\\Haskell 8.6.5\\Web\\.stack-work\\install\\a1d9d7bd\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Web_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Web_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Web_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Web_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Web_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Web_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
