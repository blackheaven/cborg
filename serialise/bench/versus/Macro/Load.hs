{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Macro.Load (readPkgIndex) where

import qualified Distribution.Version                   as Cabal
import qualified Distribution.PackageDescription        as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec                    as Cabal

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (normalise, splitDirectories, takeExtension)
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

readPkgIndex :: LBS.ByteString -> [Cabal.GenericPackageDescription]
readPkgIndex = extractCabalFiles . readTarIndex
  where
    extractCabalFiles :: [Tar.Entry] -> [Cabal.GenericPackageDescription]
    extractCabalFiles entries =
      [ pkgDesc
      | entry@Tar.Entry {
          Tar.entryContent = Tar.NormalFile cabalFile _
        } <- entries
      , let filename = Tar.entryPath entry
      , takeExtension filename == ".cabal"
      , pkgDesc <- either (const []) (\pkg -> [pkg])
                 . snd
                 . Cabal.runParseResult
                 . Cabal.parseGenericPackageDescription
                 . LBS.toStrict
                 $ cabalFile
      ]

    readTarIndex :: LBS.ByteString -> [Tar.Entry]
    readTarIndex = filter isCabalFileEntry
                 . Tar.foldEntries (:) [] throw
                 . Tar.read
      where
        isCabalFileEntry e
          | [_pkgname,versionStr,_] <- splitDirectories (normalise (Tar.entryPath e))
          , Just (_ :: Cabal.Version) <- Cabal.simpleParsec versionStr
          = True
        isCabalFileEntry _ = False
