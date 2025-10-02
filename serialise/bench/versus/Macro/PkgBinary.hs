{-# OPTIONS_GHC -fno-warn-orphans -fsimpl-tick-factor=500 #-}
module Macro.PkgBinary where

import Distribution.PackageDescription

import Data.Binary as Binary
import Data.Binary.Get as Binary
import Data.ByteString.Lazy as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise pkgs = Binary.encode pkgs

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = Binary.decode

deserialiseNull :: BS.ByteString -> ()
deserialiseNull =
    Binary.runGet $ do
      n <- get :: Get Int
      go n
  where
    go 0 = return ()
    go i = do x <- get :: Get GenericPackageDescription
              x `seq` go (i-1)

-- The Cabal-syntax package provides generic derived instances.

