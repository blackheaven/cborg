{-# LANGUAGE BangPatterns #-}
-- For PackageDescription and friends
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- For encodeCtrN/decodeCtrBodyN/etc
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Macro.CBOR (serialise, deserialise, deserialiseNull) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.System
import Distribution.License
import qualified Distribution.SPDX as SPDX
import Distribution.Version
import Distribution.ModuleName
import Distribution.Utils.ShortText
import Distribution.Utils.Path
import Distribution.Compat.NonEmptySet
import Language.Haskell.Extension

import Codec.Serialise.Class
import Codec.Serialise.Decoding hiding (DecodeAction(Done, Fail))
import Codec.CBOR.Read
import Codec.CBOR.Write

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS

import Control.Exception (throw)

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise = BS.toLazyByteString . toBuilder . encode

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = either throw snd . deserialiseFromBytes decode

deserialiseNull :: BS.ByteString -> ()
deserialiseNull = either throw snd . deserialiseFromBytes decodeListNull
  where
    decodeListNull :: Decoder s ()
    decodeListNull = do decodeListLenIndef; go

    go = do stop <- decodeBreakOr
            if stop then return ()
                    else do !_ <- decode :: Decoder s GenericPackageDescription
                            go
-- Generic or derived instances

instance Serialise Version
instance Serialise ShortText
instance Serialise PackageName
instance Serialise PackageIdentifier
instance Serialise VersionRange
instance Serialise Dependency
instance Serialise CompilerFlavor
instance Serialise a => Serialise (PerCompilerFlavor a)
instance Serialise License
instance Serialise SPDX.License
instance Serialise SPDX.LicenseExpression
instance Serialise SPDX.SimpleLicenseExpression
instance Serialise SPDX.LicenseRef
instance Serialise SourceRepo
instance Serialise RepoKind
instance Serialise RepoType
instance Serialise KnownRepoType
instance Serialise BuildType
instance Serialise Library
instance Serialise LibraryName
instance Serialise LibraryVisibility
instance Serialise Executable
instance Serialise ExecutableScope
instance Serialise TestSuite
instance Serialise TestSuiteInterface
instance Serialise TestType
instance Serialise Benchmark
instance Serialise BenchmarkInterface
instance Serialise BenchmarkType
instance Serialise ForeignLib
instance Serialise ForeignLibType
instance Serialise ForeignLibOption
instance Serialise LibVersionInfo
instance Serialise BuildInfo
instance Serialise UnqualComponentName
instance Serialise SetupBuildInfo
instance Serialise ExeDependency
instance Serialise LegacyExeDependency
instance Serialise PkgconfigDependency
instance Serialise PkgconfigName
instance Serialise PkgconfigVersionRange
instance Serialise PkgconfigVersion
instance Serialise ModuleName
instance Serialise ModuleReexport
instance Serialise ModuleRenaming
instance Serialise Mixin
instance Serialise IncludeRenaming
instance Serialise Language
instance Serialise Extension
instance Serialise KnownExtension
instance Serialise PackageDescription
instance Serialise CabalSpecVersion
instance Serialise OS
instance Serialise Arch
instance Serialise PackageFlag
instance Serialise FlagName
instance (Serialise a, Serialise b, Serialise c) => Serialise (CondTree a b c)
instance (Serialise a, Serialise b, Serialise c) => Serialise (CondBranch a b c)
instance Serialise ConfVar
instance Serialise a => Serialise (Condition a)
instance Serialise GenericPackageDescription

-- Custom instances

instance Serialise (SymbolicPath f t) where
  encode = encode . getSymbolicPath
  decode = fmap unsafeMakeSymbolicPath decode

instance Serialise SPDX.LicenseId where
  encode = encode . fromEnum
  decode = fmap toEnum decode

instance Serialise SPDX.LicenseExceptionId where
  encode = encode . fromEnum
  decode = fmap toEnum decode

instance (Serialise a, Ord a) => Serialise (NonEmptySet a) where
  encode = encode . toNonEmpty
  decode = fmap fromNonEmpty decode

