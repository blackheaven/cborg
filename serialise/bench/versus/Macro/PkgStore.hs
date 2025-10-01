{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Macro.PkgStore where

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

import Data.Functor.Contravariant
import Data.ByteString as BS
import Data.Store as Store

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Store.encode pkgs

deserialise :: ByteString -> [GenericPackageDescription]
deserialise = (\(Right x) -> x) . Store.decode


-- Generic or derived instances

instance Store Version
instance Store ShortText
instance Store PackageName
instance Store PackageIdentifier
instance Store VersionRange
instance Store Dependency
instance Store CompilerFlavor
instance Store a => Store (PerCompilerFlavor a)
instance Store License
instance Store SPDX.License
instance Store SPDX.LicenseExpression
instance Store SPDX.SimpleLicenseExpression
instance Store SPDX.LicenseRef
instance Store SourceRepo
instance Store RepoKind
instance Store RepoType
instance Store KnownRepoType
instance Store BuildType
instance Store Library
instance Store LibraryName
instance Store LibraryVisibility
instance Store Executable
instance Store ExecutableScope
instance Store TestSuite
instance Store TestSuiteInterface
instance Store TestType
instance Store Benchmark
instance Store BenchmarkInterface
instance Store BenchmarkType
instance Store ForeignLib
instance Store ForeignLibType
instance Store ForeignLibOption
instance Store LibVersionInfo
instance Store BuildInfo
instance Store UnqualComponentName
instance Store SetupBuildInfo
instance Store ExeDependency
instance Store LegacyExeDependency
instance Store PkgconfigDependency
instance Store PkgconfigName
instance Store PkgconfigVersionRange
instance Store PkgconfigVersion
instance Store ModuleName
instance Store ModuleReexport
instance Store ModuleRenaming
instance Store Mixin
instance Store IncludeRenaming
instance Store Language
instance Store Extension
instance Store KnownExtension
instance Store PackageDescription
instance Store CabalSpecVersion
instance Store OS
instance Store Arch
instance Store PackageFlag
instance Store FlagName
instance (Store a, Store b, Store c) => Store (CondTree a b c)
instance (Store a, Store b, Store c) => Store (CondBranch a b c)
instance Store ConfVar
instance Store a => Store (Condition a)
instance Store GenericPackageDescription

-- Custom instances

#if MIN_VERSION_Cabal_syntax(3,14,0)
instance Store (SymbolicPathX r f t) where
#else
instance Store (SymbolicPath f t) where
#endif
  size = contramap getSymbolicPath size
  poke = poke . getSymbolicPath
  peek = fmap unsafeMakeSymbolicPath peek

instance Store SPDX.LicenseId where
  size = contramap fromEnum size
  poke = poke . fromEnum
  peek = fmap toEnum peek

instance Store SPDX.LicenseExceptionId where
  size = contramap fromEnum size 
  poke = poke . fromEnum
  peek = fmap toEnum peek

instance (Store a, Ord a) => Store (NonEmptySet a) where
  size = contramap toNonEmpty size
  poke = poke . toNonEmpty
  peek = fmap fromNonEmpty peek

