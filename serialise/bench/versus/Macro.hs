{-# LANGUAGE CPP #-}
module Macro
  ( benchmarks -- :: [Benchmark]
  ) where
import           Data.Int

import           Criterion.Main
import           Control.DeepSeq
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BS
import qualified Codec.Compression.GZip as GZip

import Distribution.PackageDescription (GenericPackageDescription)
import qualified Macro.Load as Load

import qualified Macro.ReadShow  as ReadShow
import qualified Macro.PkgBinary as PkgBinary
import qualified Macro.PkgCereal as PkgCereal
#if __GLASGOW_HASKELL__ >= 900
-- ghc-8.x runs out of memory compiling this module:
import qualified Macro.PkgAesonGeneric as PkgAesonGeneric
#endif
import qualified Macro.PkgAesonTH as PkgAesonTH
import qualified Macro.PkgStore as PkgStore

import qualified Macro.CBOR as CBOR

readBigTestData :: IO [GenericPackageDescription]
readBigTestData =
    fmap (take 100000 . Load.readPkgIndex . GZip.decompress)
         (BS.readFile "serialise/bench/data/01-index.tar.gz")

benchmarks :: [Benchmark]
benchmarks =
  [ env readBigTestData $ \tstdata ->
    bgroup "reference"
      [ bench "deepseq" (whnf rnf tstdata)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "encoding"
      [ bench "binary"        (whnf perfEncodeBinary       tstdata)
      , bench "cereal"        (whnf perfEncodeCereal       tstdata)
#if __GLASGOW_HASKELL__ >= 900
      , bench "aeson generic" (whnf perfEncodeAesonGeneric tstdata)
#endif
      , bench "aeson TH"      (whnf perfEncodeAesonTH      tstdata)
      , bench "read/show"     (whnf perfEncodeReadShow     tstdata)
      , bench "cbor"          (whnf perfEncodeCBOR         tstdata)
      , bench "store"         (whnf perfEncodeStore        tstdata)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "decoding whnf"
      [ env (return $ combineChunks $ PkgBinary.serialise tstdata)
        $ \tstdataB -> bench "binary" (whnf perfDecodeBinary tstdataB)
      , env (return $ combineChunks $ PkgCereal.serialise tstdata)
        $ \tstdataC -> bench "cereal"  (whnf perfDecodeCereal tstdataC)
      , env (return $ combineChunks $ PkgAesonTH.serialise tstdata)
        $ \tstdataA -> bgroup "aeson"
            [ bench "TH"        (whnf perfDecodeAesonTH      tstdataA)
#if __GLASGOW_HASKELL__ >= 900
            , bench "generic"   (whnf perfDecodeAesonGeneric tstdataA)
#endif
            ]
      , env (return $ combineChunks $ ReadShow.serialise tstdata)
        $ \tstdataS -> bench "read/show" (whnf perfDecodeReadShow tstdataS)
      , env (return $ PkgStore.serialise tstdata)
        $ \tstdataR -> bench "store" (whnf perfDecodeStore tstdataR)
      , env (return $ combineChunks $ CBOR.serialise tstdata)
        $ \tstdataR -> bench "cbor" (whnf perfDecodeCBOR tstdataR)
      ]

  , env readBigTestData $ \tstdata ->
    bgroup "decoding nf"
      [ env (return $ combineChunks $ PkgBinary.serialise tstdata)
      $ \tstdataB -> bench "binary" (nf perfDecodeBinary tstdataB)
      , env (return $ combineChunks $ PkgCereal.serialise tstdata)
      $ \tstdataC -> bench "cereal" (nf perfDecodeCereal tstdataC)
      , env (return $ combineChunks $ PkgAesonTH.serialise tstdata)
      $ \tstdataA -> bgroup "aeson"
          [ bench "TH"        (nf perfDecodeAesonTH      tstdataA)
#if __GLASGOW_HASKELL__ >= 900
          , bench "generic"   (nf perfDecodeAesonGeneric tstdataA)
#endif
          ]

      , env (return $ combineChunks $ ReadShow.serialise tstdata)
      $ \tstdataS -> bench "read/show" (nf perfDecodeReadShow tstdataS)
      , env (return $ PkgStore.serialise tstdata)
      $ \tstdataR -> bench "store" (nf perfDecodeStore tstdataR)
      , env (return $ combineChunks $ CBOR.serialise tstdata)
      $ \tstdataR -> bench "cbor" (nf perfDecodeCBOR tstdataR)
      ]
  ]
  where
    perfEncodeBinary, perfEncodeCereal,
      perfEncodeAesonTH, perfEncodeReadShow,
      perfEncodeCBOR
      :: [GenericPackageDescription] -> Int64


    perfEncodeBinary       = BS.length . PkgBinary.serialise
    perfEncodeCereal       = BS.length . PkgCereal.serialise
    perfEncodeAesonTH      = BS.length . PkgAesonTH.serialise
    perfEncodeReadShow     = BS.length . ReadShow.serialise
    perfEncodeCBOR         = BS.length . CBOR.serialise
#if __GLASGOW_HASKELL__ >= 900
    perfEncodeAesonGeneric :: [GenericPackageDescription] -> Int64
    perfEncodeAesonGeneric = BS.length . PkgAesonGeneric.serialise
#endif

    perfDecodeBinary, perfDecodeCereal,
      perfDecodeAesonTH, perfDecodeReadShow,
      perfDecodeCBOR
      :: BS.ByteString -> [GenericPackageDescription]

    perfDecodeBinary       = PkgBinary.deserialise
    perfDecodeCereal       = PkgCereal.deserialise
    perfDecodeAesonTH      = PkgAesonTH.deserialise
    perfDecodeReadShow     = ReadShow.deserialise
    perfDecodeCBOR        = CBOR.deserialise
#if __GLASGOW_HASKELL__ >= 900
    perfDecodeAesonGeneric :: BS.ByteString -> [GenericPackageDescription]
    perfDecodeAesonGeneric = PkgAesonGeneric.deserialise
#endif

    perfDecodeStore :: B.ByteString -> [GenericPackageDescription]
    perfDecodeStore = PkgStore.deserialise
    perfEncodeStore :: [GenericPackageDescription] -> Int
    perfEncodeStore = B.length . PkgStore.serialise

    -- Convert any lazy ByteString to ByteString lazy bytestring
    -- that have only single chunk.
    combineChunks :: BS.ByteString -> BS.ByteString
    combineChunks = BS.fromStrict . BS.toStrict
