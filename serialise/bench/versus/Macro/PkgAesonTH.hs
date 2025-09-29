{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
module Macro.PkgAesonTH where

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
import Data.Aeson.TH as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Typeable
import qualified Data.Text.Encoding as T

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

deriving via ViaPrettyParsec PkgconfigVersion
 instance ToJSON PkgconfigVersion

deriving via ViaPrettyParsec PkgconfigVersion
 instance FromJSON PkgconfigVersion

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

-- TH derived instances

-- Some of these types are mutually recursive, so we use a single splice.
-- This also avoids having to order topoligically.
$(concat <$> mapM (deriveJSON defaultOptions)
    [ ''Version
    , ''PackageName
    , ''PackageIdentifier
    , ''VersionRange
    , ''UnqualComponentName
    , ''LibraryName
    , ''Dependency
    , ''CompilerFlavor
    , ''PerCompilerFlavor
    , ''License
    , ''SPDX.LicenseId
    , ''SPDX.LicenseExceptionId
    , ''SPDX.LicenseRef
    , ''SPDX.SimpleLicenseExpression
    , ''SPDX.LicenseExpression
    , ''SPDX.License
    , ''RepoKind
    , ''KnownRepoType
    , ''RepoType
    , ''SourceRepo
    , ''BuildType
    , ''ModuleName
    , ''ModuleReexport
    , ''LibraryVisibility
    , ''ExecutableScope
    , ''ExeDependency
    , ''LegacyExeDependency
    , ''PkgconfigName
    , ''PkgconfigVersionRange
    , ''PkgconfigDependency
    , ''ModuleRenaming
    , ''IncludeRenaming
    , ''Mixin
    , ''Language
    , ''KnownExtension
    , ''Extension
    , ''BuildInfo
    , ''Library
    , ''Executable
    , ''TestType
    , ''TestSuiteInterface
    , ''TestSuite
    , ''BenchmarkType
    , ''BenchmarkInterface
    , ''Benchmark
    , ''LibVersionInfo
    , ''ForeignLibType
    , ''ForeignLibOption
    , ''ForeignLib
    , ''SetupBuildInfo
    , ''CabalSpecVersion
    , ''OS
    , ''Arch
    , ''PackageDescription
    , ''FlagName
    , ''PackageFlag
    , ''ConfVar
    , ''Condition
    , ''CondTree
    , ''CondBranch
    , ''GenericPackageDescription
    ]
 )

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Aeson.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = fromJust . Aeson.decode'

