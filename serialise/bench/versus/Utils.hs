{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( prepBenchmarkFiles -- :: IO ()
  ) where

import           Data.Time
import           System.IO
import           System.Mem
import           Control.Monad (when)
import qualified Control.Exception as Exception
import           System.FilePath
import           System.Directory
import           Control.DeepSeq (force)

import qualified Data.ByteString.Lazy   as LBS
import qualified Codec.Compression.GZip as GZip

import qualified Macro.Load      as Load
import qualified Macro.PkgBinary as PkgBinary
import qualified Macro.PkgCereal as PkgCereal
import qualified Macro.PkgStore  as PkgStore
import qualified Macro.CBOR      as CBOR

--------------------------------------------------------------------------------

-- | Get the path to the Hackage.haskell.org package index. Works on Windows
-- and Linux.
getHackageIndexLocation :: IO FilePath
getHackageIndexLocation = do
  cabalCacheDir <- getXdgDirectory XdgCache "cabal"
  let dir = cabalCacheDir </> "packages" </> "hackage.haskell.org"
  return (dir </> "01-index.tar.gz")

-- | Copy the hackage index to a local directory. Returns the path to the
-- directory containing the file, and the file path itself.
copyHackageIndex :: IO (FilePath, FilePath)
copyHackageIndex = do
  hackageIndex <- getHackageIndexLocation

  let dataDir = "serialise" </> "bench" </> "data"
      dest    = dataDir </> "01-index.tar.gz"

  -- Create the data dir, and copy the index.
  -- We do not try to create the 'bench' directory since it should exist.
  createDirectoryIfMissing False dataDir
  exists <- doesFileExist dest
  when (not exists) $ do
    notice "Copying hackage index" (copyFile hackageIndex dest)

  return (dataDir, dest)

-- | Prepare all the files needed for the benchmarks to properly run,
-- including needing a copy of the Hackage index, and a few encodings
-- of some of its contents
prepBenchmarkFiles :: IO ()
prepBenchmarkFiles = do
  -- Set up index
  (destDir, indexFile) <- copyHackageIndex

  -- Read it
  pkgs <- notice "parsing 150k packages" $
            Exception.evaluate
          . force
          . take 150000
          . Load.readPkgIndex
          . GZip.decompress
        =<< LBS.readFile indexFile

  -- Encode that dense data in several forms, and write those forms out
  -- to disk.
  write (destDir </> "binary.bin") (PkgBinary.serialise pkgs)
  write (destDir </> "cereal.bin") (PkgCereal.serialise pkgs)
  write (destDir </> "cbor.bin")   (CBOR.serialise      pkgs)
  writeS (destDir </> "store.bin") (PkgStore.serialise  pkgs)

  -- And before we finish: do a garbage collection to clean up anything left
  -- over.
  notice "Preparation done; performing GC" doGC

  where
    -- Write a file to the temporary directory, if it does not exist.
    write file lbs = do
      exists <- doesFileExist file
      when (not exists) $ do
        let msg = "Creating " ++ file
        notice msg (writeFileAtomic file lbs)

    -- Write a file to the temporary directory, if it does not exist.
    -- Strict version.
    writeS file = write file . LBS.fromStrict


--------------------------------------------------------------------------------

-- | Do a garbage collection.
doGC :: IO ()
doGC = performMajorGC

-- Write a notice to the screen (with timing information).
notice :: String -> IO a -> IO a
notice m k = do
  putStr ("INFO: " ++ m ++ "... ")
  hFlush stdout
  (v,t) <- timeIt k
  putStrLn $ "OK (in " ++ show t ++ ")"
  return v

-- | Time some action and return the time difference.
timeIt :: IO a -> IO (a, NominalDiffTime)
timeIt action = do
  t   <- getCurrentTime
  x   <- action
  t'  <- getCurrentTime
  return (x, diffUTCTime t' t)

writeFileAtomic :: FilePath -> LBS.ByteString -> IO ()
writeFileAtomic targetPath content = do
  let targetFile = takeFileName targetPath
  tmpDir <- getTemporaryDirectory
  Exception.bracketOnError
    (openBinaryTempFileWithDefaultPermissions tmpDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    ( \(tmpPath, handle) -> do
        LBS.hPut handle content
        hClose handle
        Exception.catch
          (renameFile tmpPath targetPath)
          ( \(_ :: Exception.SomeException) -> do
              copyFile tmpPath targetPath
              removeFile tmpPath
          )
    )

