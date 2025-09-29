{-# OPTIONS_GHC -fsimpl-tick-factor=500 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Macro.PkgCereal where

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

import Data.List.NonEmpty as NonEmpty

import Data.Serialize as Cereal
import Data.ByteString.Lazy as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Cereal.encodeLazy pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = (\(Right x) -> x) . Cereal.decodeLazy

deserialiseNull :: BS.ByteString -> ()
deserialiseNull bs =
    case Cereal.runGetLazy decodeListNull bs of
      Right () -> ()
      Left e   -> error $ "PkgCereal.deserialiseNull: decoding failed: " ++ e
  where
    decodeListNull = do
      n <- get :: Get Int
      go n
    go 0 = return ()
    go i = do x <- get :: Get GenericPackageDescription
              x `seq` go (i-1)

-- Generic or derived instances

instance Serialize Version
instance Serialize ShortText
instance Serialize PackageName
instance Serialize PackageIdentifier
instance Serialize VersionRange
instance Serialize Dependency
instance Serialize CompilerFlavor
instance Serialize a => Serialize (PerCompilerFlavor a)
instance Serialize License
instance Serialize SPDX.License
instance Serialize SPDX.LicenseExpression
instance Serialize SPDX.SimpleLicenseExpression
instance Serialize SPDX.LicenseRef
instance Serialize SourceRepo
instance Serialize RepoKind
instance Serialize RepoType
instance Serialize KnownRepoType
instance Serialize BuildType
instance Serialize Library
instance Serialize LibraryName
instance Serialize LibraryVisibility
instance Serialize Executable
instance Serialize ExecutableScope
instance Serialize TestSuite
instance Serialize TestSuiteInterface
instance Serialize TestType
instance Serialize Benchmark
instance Serialize BenchmarkInterface
instance Serialize BenchmarkType
instance Serialize ForeignLib
instance Serialize ForeignLibType
instance Serialize ForeignLibOption
instance Serialize LibVersionInfo
instance Serialize BuildInfo
instance Serialize UnqualComponentName
instance Serialize SetupBuildInfo
instance Serialize ExeDependency
instance Serialize LegacyExeDependency
instance Serialize PkgconfigDependency
instance Serialize PkgconfigName
instance Serialize PkgconfigVersionRange
instance Serialize PkgconfigVersion
instance Serialize ModuleName
instance Serialize ModuleReexport
instance Serialize ModuleRenaming
instance Serialize Mixin
instance Serialize IncludeRenaming
instance Serialize Language
instance Serialize Extension
instance Serialize KnownExtension
instance Serialize PackageDescription
instance Serialize CabalSpecVersion
instance Serialize OS
instance Serialize Arch
instance Serialize PackageFlag
instance Serialize FlagName
instance (Serialize a, Serialize b, Serialize c) => Serialize (CondTree a b c)
instance (Serialize a, Serialize b, Serialize c) => Serialize (CondBranch a b c)
instance Serialize ConfVar
instance Serialize a => Serialize (Condition a)
instance Serialize GenericPackageDescription

-- Custom instances

instance Serialize (SymbolicPath f t) where
  put = put . getSymbolicPath
  get = fmap unsafeMakeSymbolicPath get

instance Serialize SPDX.LicenseId where
  put = put . fromEnum
  get = fmap toEnum get

instance Serialize SPDX.LicenseExceptionId where
  put = put . fromEnum
  get = fmap toEnum get

instance (Serialize a, Ord a) => Serialize (NonEmptySet a) where
  put = put . NonEmpty.toList . toNonEmpty
  get = fmap (fromNonEmpty . NonEmpty.fromList) get

