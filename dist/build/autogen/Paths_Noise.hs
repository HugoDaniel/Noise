module Paths_Noise (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/hugo/.cabal/bin"
libdir     = "/home/hugo/.cabal/lib/Noise-0.0.1/ghc-6.12.1"
datadir    = "/home/hugo/.cabal/share/Noise-0.0.1"
libexecdir = "/home/hugo/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Noise_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Noise_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Noise_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Noise_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
