module Test.CompactFilter
    ( genBlockFilter
    ) where

import           Control.Monad                (replicateM)
import           Haskoin.Constants            (btc)
import           Haskoin.Util.Arbitrary.Block (arbitraryBlock)
import           Haskoin.Util.Arbitrary.Util  (arbitraryBS1)
import           Test.Tasty.QuickCheck        (Gen, choose)

import           Bitcoin.CompactFilter        (BlockFilter, encodeFilter)


genBlockFilter :: Gen BlockFilter
genBlockFilter = encodeFilter <$> scripts <*> arbitraryBlock btc
    where
    scripts = do
        n <- choose (1, 100)
        replicateM n arbitraryBS1
