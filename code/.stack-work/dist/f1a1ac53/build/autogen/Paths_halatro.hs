{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_halatro (
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
bindir     = "C:\\Users\\feran\\Documents\\University\\Year1\\Coursework\\CS141\\Coursework 1\\.stack-work\\install\\5c35a4f7\\bin"
libdir     = "C:\\Users\\feran\\Documents\\University\\Year1\\Coursework\\CS141\\Coursework 1\\.stack-work\\install\\5c35a4f7\\lib\\x86_64-windows-ghc-9.4.8\\halatro-0.1.0.0-LSKNsvv02FeFWtYVlKYCUk"
dynlibdir  = "C:\\Users\\feran\\Documents\\University\\Year1\\Coursework\\CS141\\Coursework 1\\.stack-work\\install\\5c35a4f7\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "C:\\Users\\feran\\Documents\\University\\Year1\\Coursework\\CS141\\Coursework 1\\.stack-work\\install\\5c35a4f7\\share\\x86_64-windows-ghc-9.4.8\\halatro-0.1.0.0"
libexecdir = "C:\\Users\\feran\\Documents\\University\\Year1\\Coursework\\CS141\\Coursework 1\\.stack-work\\install\\5c35a4f7\\libexec\\x86_64-windows-ghc-9.4.8\\halatro-0.1.0.0"
sysconfdir = "C:\\Users\\feran\\Documents\\University\\Year1\\Coursework\\CS141\\Coursework 1\\.stack-work\\install\\5c35a4f7\\etc"

getBinDir     = catchIO (getEnv "halatro_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "halatro_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "halatro_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "halatro_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "halatro_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "halatro_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
