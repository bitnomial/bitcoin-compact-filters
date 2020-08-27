module Test.Examples.Mainnet
    ( examples
    ) where

import           Data.Aeson                    (eitherDecodeFileStrict)

import           Paths_bitcoin_compact_filters (getDataFileName)
import           Test.Util                     (Example)


examples :: IO [Example]
examples = getDataFileName "examples/examples.json" >>= eitherDecodeFileStrict >>= either error return
