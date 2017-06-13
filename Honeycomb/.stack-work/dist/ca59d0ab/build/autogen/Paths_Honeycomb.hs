{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Honeycomb (
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

bindir     = "D:\\workspace\\spop\\Honeycomb\\.stack-work\\install\\fdf25839\\bin"
libdir     = "D:\\workspace\\spop\\Honeycomb\\.stack-work\\install\\fdf25839\\lib\\x86_64-windows-ghc-8.0.2\\Honeycomb-0.1.0.0-Fox2yB5lKpb2Q7x8zLKqUX"
dynlibdir  = "D:\\workspace\\spop\\Honeycomb\\.stack-work\\install\\fdf25839\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "D:\\workspace\\spop\\Honeycomb\\.stack-work\\install\\fdf25839\\share\\x86_64-windows-ghc-8.0.2\\Honeycomb-0.1.0.0"
libexecdir = "D:\\workspace\\spop\\Honeycomb\\.stack-work\\install\\fdf25839\\libexec"
sysconfdir = "D:\\workspace\\spop\\Honeycomb\\.stack-work\\install\\fdf25839\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Honeycomb_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Honeycomb_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Honeycomb_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Honeycomb_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Honeycomb_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Honeycomb_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
