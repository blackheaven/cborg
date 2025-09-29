{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.ReadShow where

import Distribution.PackageDescription

import Data.ByteString.Lazy.Char8 as BS

serialise :: [GenericPackageDescription] -> BS.ByteString
serialise = BS.pack . show

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = read . BS.unpack

deriving instance Read PackageFlag
deriving instance (Read a, Read b, Read c) => Read (CondTree a b c)
deriving instance (Read a, Read b, Read c) => Read (CondBranch a b c)
deriving instance Read ConfVar
deriving instance Read a => Read (Condition a)
deriving instance Read GenericPackageDescription
