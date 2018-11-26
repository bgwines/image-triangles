{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_image_triangles (
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

bindir     = "/Users/brett/code/image-triangles/.stack-work/install/x86_64-osx/lts-12.19/8.4.4/bin"
libdir     = "/Users/brett/code/image-triangles/.stack-work/install/x86_64-osx/lts-12.19/8.4.4/lib/x86_64-osx-ghc-8.4.4/image-triangles-0.1.0.0-BiEiu2CzRvH4EcZBOTYVL9"
dynlibdir  = "/Users/brett/code/image-triangles/.stack-work/install/x86_64-osx/lts-12.19/8.4.4/lib/x86_64-osx-ghc-8.4.4"
datadir    = "/Users/brett/code/image-triangles/.stack-work/install/x86_64-osx/lts-12.19/8.4.4/share/x86_64-osx-ghc-8.4.4/image-triangles-0.1.0.0"
libexecdir = "/Users/brett/code/image-triangles/.stack-work/install/x86_64-osx/lts-12.19/8.4.4/libexec/x86_64-osx-ghc-8.4.4/image-triangles-0.1.0.0"
sysconfdir = "/Users/brett/code/image-triangles/.stack-work/install/x86_64-osx/lts-12.19/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "image_triangles_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "image_triangles_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "image_triangles_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "image_triangles_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "image_triangles_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "image_triangles_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
