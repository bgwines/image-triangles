module Paths_image_triangles (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/brett/.cabal/bin"
libdir     = "/Users/brett/.cabal/lib/x86_64-osx-ghc-8.2.2/image-triangles-0.1.0.0-55R8CuFPEcl2QYA7Bk8WV3"
datadir    = "/Users/brett/.cabal/share/x86_64-osx-ghc-8.2.2/image-triangles-0.1.0.0"
libexecdir = "/Users/brett/.cabal/libexec"
sysconfdir = "/Users/brett/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "image_triangles_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "image_triangles_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "image_triangles_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "image_triangles_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "image_triangles_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
