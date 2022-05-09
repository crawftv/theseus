{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_server (
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

bindir     = "/Users/crawford.collins/OneDrive/theseus/.stack-work/install/x86_64-osx/e6e282c5ae095aca8896ce3cdddaffbf7a279fee6508fded6a9864cfbcdbb023/8.10.7/bin"
libdir     = "/Users/crawford.collins/OneDrive/theseus/.stack-work/install/x86_64-osx/e6e282c5ae095aca8896ce3cdddaffbf7a279fee6508fded6a9864cfbcdbb023/8.10.7/lib/x86_64-osx-ghc-8.10.7/server-0.1.0.0-271GhbaWrqB2qMTIbss0Qh"
dynlibdir  = "/Users/crawford.collins/OneDrive/theseus/.stack-work/install/x86_64-osx/e6e282c5ae095aca8896ce3cdddaffbf7a279fee6508fded6a9864cfbcdbb023/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/crawford.collins/OneDrive/theseus/.stack-work/install/x86_64-osx/e6e282c5ae095aca8896ce3cdddaffbf7a279fee6508fded6a9864cfbcdbb023/8.10.7/share/x86_64-osx-ghc-8.10.7/server-0.1.0.0"
libexecdir = "/Users/crawford.collins/OneDrive/theseus/.stack-work/install/x86_64-osx/e6e282c5ae095aca8896ce3cdddaffbf7a279fee6508fded6a9864cfbcdbb023/8.10.7/libexec/x86_64-osx-ghc-8.10.7/server-0.1.0.0"
sysconfdir = "/Users/crawford.collins/OneDrive/theseus/.stack-work/install/x86_64-osx/e6e282c5ae095aca8896ce3cdddaffbf7a279fee6508fded6a9864cfbcdbb023/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
