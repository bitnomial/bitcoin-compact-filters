{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Bitcoin.CompactFilter (
    blockFilterHeader,
    encodeFilter,
    filterContents,
    isMember,
 )
import Data.Serialize (decode, encode)
import Haskoin.Block (Block (..), headerHash)
import Haskoin.Crypto (Ctx (..), withContext)
import Test.CompactFilter (genBlockFilter)
import qualified Test.Examples.Bip as BIP
import qualified Test.Examples.Mainnet as Mainnet
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (elements, forAll, testProperty, (===))
import Test.Util (Example (..))

main :: IO ()
main = do
    bipExamples <- BIP.examples
    mainnetExamples <- Mainnet.examples
    withContext $ \ctx ->
        defaultMain . testGroup "bip158 unit tests" $
            [ testGroup "Serialization" [testRoundtrip ctx]
            , testGroup
                "Filter"
                [ testGroup "BIP 158" $ testFilter <$> bipExamples
                , testGroup "Mainnet" $ testFilter <$> mainnetExamples
                ]
            ]

testRoundtrip :: Ctx -> TestTree
testRoundtrip ctx = testProperty "serialization round trip" . forAll (genBlockFilter ctx) $
    \bf -> decode (encode bf) === Right bf

testFilter :: Example -> TestTree
testFilter v = testGroup (testLabel v) $ [construction] <> [membership | not (null ss)]
  where
    construction = testCase "Construction" $ do
        ourFilter @?= exampleFilter v
        ourHeader @?= exampleHeader v

    membership = testProperty "Filter membership" $
        forAll (elements ss) $ \s ->
            isMember bh [s] ourFilter

    ourFilter = encodeFilter (examplePrevOuts v) block
    ourHeader = blockFilterHeader (examplePrevHeader v) ourFilter
    bh = headerHash block.header
    ss = filterContents (examplePrevOuts v) block

    block = exampleBlock v

testLabel :: Example -> String
testLabel e = "Block " <> show (exampleHeight e)
