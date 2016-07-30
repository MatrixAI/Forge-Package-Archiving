{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_http_server (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [1,0,6] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/quoch/haskell/http-server/.cabal-sandbox/bin"
libdir     = "/home/quoch/haskell/http-server/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.1/http-server-1.0.6-5nwb4VEBqq9Lj6sL7kMrrv"
datadir    = "/home/quoch/haskell/http-server/.cabal-sandbox/share/x86_64-linux-ghc-8.0.1/http-server-1.0.6"
libexecdir = "/home/quoch/haskell/http-server/.cabal-sandbox/libexec"
sysconfdir = "/home/quoch/haskell/http-server/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "http_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "http_server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "http_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "http_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "http_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
