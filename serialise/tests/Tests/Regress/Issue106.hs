module Tests.Regress.Issue106 ( testTree ) where

import qualified Codec.Serialise as Serialise
import qualified Codec.CBOR.Pretty as CBOR
import           Test.Tasty
import           Test.Tasty.HUnit

repro :: String
repro =
    CBOR.prettyHexEnc $ Serialise.encode (5 :: Word)

testTree :: TestTree
testTree =
    testGroup "Issue 106 - Pretty-printing of Word"
        [ testCase "simple reproduction case"   ("\n05  # word(5)" @=? repro) ]
