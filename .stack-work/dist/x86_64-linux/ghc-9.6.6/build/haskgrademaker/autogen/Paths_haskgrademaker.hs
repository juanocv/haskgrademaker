{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskgrademaker (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/gabriel/Documentos/haskgrademaker/.stack-work/install/x86_64-linux/c8e5f0c45c14c0baf0cf732da3dad0afe151506d8a130f8ef5b5f237ca1aa5eb/9.6.6/bin"
libdir     = "/home/gabriel/Documentos/haskgrademaker/.stack-work/install/x86_64-linux/c8e5f0c45c14c0baf0cf732da3dad0afe151506d8a130f8ef5b5f237ca1aa5eb/9.6.6/lib/x86_64-linux-ghc-9.6.6/haskgrademaker-0.1.0.0-4oRSWRqbLtMG4ZPhhAWk3J-haskgrademaker"
dynlibdir  = "/home/gabriel/Documentos/haskgrademaker/.stack-work/install/x86_64-linux/c8e5f0c45c14c0baf0cf732da3dad0afe151506d8a130f8ef5b5f237ca1aa5eb/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/gabriel/Documentos/haskgrademaker/.stack-work/install/x86_64-linux/c8e5f0c45c14c0baf0cf732da3dad0afe151506d8a130f8ef5b5f237ca1aa5eb/9.6.6/share/x86_64-linux-ghc-9.6.6/haskgrademaker-0.1.0.0"
libexecdir = "/home/gabriel/Documentos/haskgrademaker/.stack-work/install/x86_64-linux/c8e5f0c45c14c0baf0cf732da3dad0afe151506d8a130f8ef5b5f237ca1aa5eb/9.6.6/libexec/x86_64-linux-ghc-9.6.6/haskgrademaker-0.1.0.0"
sysconfdir = "/home/gabriel/Documentos/haskgrademaker/.stack-work/install/x86_64-linux/c8e5f0c45c14c0baf0cf732da3dad0afe151506d8a130f8ef5b5f237ca1aa5eb/9.6.6/etc"

getBinDir     = catchIO (getEnv "haskgrademaker_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskgrademaker_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskgrademaker_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskgrademaker_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskgrademaker_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskgrademaker_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
