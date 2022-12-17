{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_HCodecs (
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
version = Version [0,5,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/lazylambda/.cabal/store/ghc-8.10.7/HCodecs-0.5.2-7dd42688c4d8ffa2e17ea26e7da3a6bf2a0df52ef474924cd509be4e26ea3de4/bin"
libdir     = "/home/lazylambda/.cabal/store/ghc-8.10.7/HCodecs-0.5.2-7dd42688c4d8ffa2e17ea26e7da3a6bf2a0df52ef474924cd509be4e26ea3de4/lib"
dynlibdir  = "/home/lazylambda/.cabal/store/ghc-8.10.7/HCodecs-0.5.2-7dd42688c4d8ffa2e17ea26e7da3a6bf2a0df52ef474924cd509be4e26ea3de4/lib"
datadir    = "/home/lazylambda/.cabal/store/ghc-8.10.7/HCodecs-0.5.2-7dd42688c4d8ffa2e17ea26e7da3a6bf2a0df52ef474924cd509be4e26ea3de4/share"
libexecdir = "/home/lazylambda/.cabal/store/ghc-8.10.7/HCodecs-0.5.2-7dd42688c4d8ffa2e17ea26e7da3a6bf2a0df52ef474924cd509be4e26ea3de4/libexec"
sysconfdir = "/home/lazylambda/.cabal/store/ghc-8.10.7/HCodecs-0.5.2-7dd42688c4d8ffa2e17ea26e7da3a6bf2a0df52ef474924cd509be4e26ea3de4/etc"

getBinDir     = catchIO (getEnv "HCodecs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "HCodecs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "HCodecs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "HCodecs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HCodecs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HCodecs_sysconfdir") (\_ -> return sysconfdir)




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
