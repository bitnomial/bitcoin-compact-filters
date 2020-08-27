{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Examples.Bip
    ( examples
    ) where

import           Data.Aeson                    (eitherDecodeFileStrict)
import           Data.Text                     (Text)
import           Haskoin.Block                 (BlockHeight)

import           Bitcoin.CompactFilter         (filterHeaderFromHex)
import           Paths_bitcoin_compact_filters (getDataFileName)
import           Test.Util                     (Example (..), decodeFromHex,
                                                parseHex)


examples :: IO [Example]
examples
    = getDataFileName "examples/testnet-19.json"
    >>= eitherDecodeFileStrict
    >>= either error return . (>>= traverse interpretRawVector)


interpretRawVector
    :: (BlockHeight, Text, Text, [Text], Text, Text, Text, Text)
    -> Either String Example
interpretRawVector (n, h, b, ss, ph, f, fh, _)
    = Example n <$> decodeFromHex h
                <*> decodeFromHex b
                <*> traverse parseHex ss
                <*> decodeFromHex f
                <*> filterHeaderFromHex ph
                <*> filterHeaderFromHex fh
