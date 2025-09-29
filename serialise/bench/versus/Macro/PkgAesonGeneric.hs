{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Macro.PkgAesonGeneric where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.System
import Distribution.License
import qualified Distribution.SPDX as SPDX
import Distribution.Version
import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import Distribution.Pretty
import Distribution.Utils.ShortText
import Distribution.Utils.Path
import Distribution.Compat.NonEmptySet
import Language.Haskell.Extension

import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Typeable
import qualified Data.Text.Encoding as T


serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Aeson.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = fromJust . Aeson.decode'


-- Generic or derived instances

instance ToJSON Version
instance ToJSON PackageName
instance ToJSON PackageIdentifier
instance ToJSON VersionRange
instance ToJSON Dependency
instance ToJSON CompilerFlavor
instance ToJSON a => ToJSON (PerCompilerFlavor a)
instance ToJSON License
instance ToJSON SPDX.License
instance ToJSON SPDX.LicenseExpression
instance ToJSON SPDX.SimpleLicenseExpression
instance ToJSON SPDX.LicenseRef
instance ToJSON SourceRepo
instance ToJSON RepoKind
instance ToJSON RepoType
instance ToJSON KnownRepoType
instance ToJSON BuildType
instance ToJSON Library
instance ToJSON LibraryName
instance ToJSON LibraryVisibility
instance ToJSON Executable
instance ToJSON ExecutableScope
instance ToJSON TestSuite
instance ToJSON TestSuiteInterface
instance ToJSON TestType
instance ToJSON Benchmark
instance ToJSON BenchmarkInterface
instance ToJSON BenchmarkType
instance ToJSON ForeignLib
instance ToJSON ForeignLibType
instance ToJSON ForeignLibOption
instance ToJSON LibVersionInfo
instance ToJSON BuildInfo
instance ToJSON UnqualComponentName
instance ToJSON SetupBuildInfo
instance ToJSON ExeDependency
instance ToJSON LegacyExeDependency
instance ToJSON PkgconfigDependency
instance ToJSON PkgconfigName
instance ToJSON PkgconfigVersionRange
deriving via ViaPrettyParsec PkgconfigVersion
 instance ToJSON PkgconfigVersion
instance ToJSON ModuleName
instance ToJSON ModuleReexport
instance ToJSON ModuleRenaming
instance ToJSON Mixin
instance ToJSON IncludeRenaming
instance ToJSON Language
instance ToJSON Extension
instance ToJSON KnownExtension
instance ToJSON PackageDescription
instance ToJSON CabalSpecVersion
instance ToJSON OS
instance ToJSON Arch
instance ToJSON PackageFlag
instance ToJSON FlagName
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (CondTree a b c)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (CondBranch a b c)
instance ToJSON ConfVar
instance ToJSON a => ToJSON (Condition a)
instance ToJSON GenericPackageDescription

-- Generic or derived instances

instance FromJSON Version
instance FromJSON PackageName
instance FromJSON PackageIdentifier
instance FromJSON VersionRange
instance FromJSON Dependency
instance FromJSON CompilerFlavor
instance FromJSON a => FromJSON (PerCompilerFlavor a)
instance FromJSON License
instance FromJSON SPDX.License
instance FromJSON SPDX.LicenseExpression
instance FromJSON SPDX.SimpleLicenseExpression
instance FromJSON SPDX.LicenseRef
instance FromJSON SourceRepo
instance FromJSON RepoKind
instance FromJSON RepoType
instance FromJSON KnownRepoType
instance FromJSON BuildType
instance FromJSON Library
instance FromJSON LibraryName
instance FromJSON LibraryVisibility
instance FromJSON Executable
instance FromJSON ExecutableScope
instance FromJSON TestSuite
instance FromJSON TestSuiteInterface
instance FromJSON TestType
instance FromJSON Benchmark
instance FromJSON BenchmarkInterface
instance FromJSON BenchmarkType
instance FromJSON ForeignLib
instance FromJSON ForeignLibType
instance FromJSON ForeignLibOption
instance FromJSON LibVersionInfo
instance FromJSON BuildInfo
instance FromJSON UnqualComponentName
instance FromJSON SetupBuildInfo
instance FromJSON ExeDependency
instance FromJSON LegacyExeDependency
instance FromJSON PkgconfigDependency
instance FromJSON PkgconfigName
instance FromJSON PkgconfigVersionRange
deriving via ViaPrettyParsec PkgconfigVersion
 instance FromJSON PkgconfigVersion
instance FromJSON ModuleName
instance FromJSON ModuleReexport
instance FromJSON ModuleRenaming
instance FromJSON Mixin
instance FromJSON IncludeRenaming
instance FromJSON Language
instance FromJSON Extension
instance FromJSON KnownExtension
instance FromJSON PackageDescription
instance FromJSON CabalSpecVersion
instance FromJSON OS
instance FromJSON Arch
instance FromJSON PackageFlag
instance FromJSON FlagName
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (CondTree a b c)
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (CondBranch a b c)
instance FromJSON ConfVar
instance FromJSON a => FromJSON (Condition a)
instance FromJSON GenericPackageDescription

-- Custom instances

instance ToJSON ShortText where
  toJSON = toJSON . fromShortText

instance FromJSON ShortText where
  parseJSON = fmap toShortText . parseJSON

instance ToJSON a => ToJSON (NonEmptySet a) where
  toJSON = toJSON . toNonEmpty

instance (FromJSON a, Ord a) => FromJSON (NonEmptySet a) where
  parseJSON = fmap fromNonEmpty . parseJSON

instance ToJSON (SymbolicPath f t) where
  toJSON = toJSON . getSymbolicPath

instance FromJSON (SymbolicPath f t) where
  parseJSON = fmap unsafeMakeSymbolicPath . parseJSON

deriving via ViaPrettyParsec SPDX.LicenseId
 instance ToJSON SPDX.LicenseId

deriving via ViaPrettyParsec SPDX.LicenseId
 instance FromJSON SPDX.LicenseId

deriving via ViaPrettyParsec SPDX.LicenseExceptionId
 instance ToJSON SPDX.LicenseExceptionId

deriving via ViaPrettyParsec SPDX.LicenseExceptionId
 instance FromJSON SPDX.LicenseExceptionId


-- For use with deriving via:

newtype ViaPrettyParsec a = ViaPrettyParsec a

instance Pretty a => ToJSON (ViaPrettyParsec a) where
  toJSON (ViaPrettyParsec x) = toJSON (prettyShow x)

instance (Parsec a, Typeable a) => FromJSON (ViaPrettyParsec a) where
  parseJSON =
      (fmap ViaPrettyParsec .) $
      withText name $
        either (Aeson.parseFail . show) return
      . runParsecParser lexemeParsec "FromJSON"
      . fieldLineStreamFromBS
      . T.encodeUtf8
    where
      name = showsTypeRep (typeRep (Proxy :: Proxy a)) ""

