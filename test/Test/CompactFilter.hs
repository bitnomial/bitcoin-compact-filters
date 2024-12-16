module Test.CompactFilter (
    genBlockFilter,
) where

import Bitcoin.CompactFilter (BlockFilter, encodeFilter)
import Control.Monad (replicateM)
import Haskoin.Crypto (Ctx)
import Haskoin.Network.Constants (btc)
import Haskoin.Util.Arbitrary.Block (arbitraryBlock)
import Haskoin.Util.Arbitrary.Util (arbitraryBS1)
import Test.Tasty.QuickCheck (Gen, choose)

genBlockFilter :: Ctx -> Gen BlockFilter
genBlockFilter ctx = encodeFilter <$> scripts <*> arbitraryBlock btc ctx
  where
    scripts = do
        n <- choose (1, 100)
        replicateM n arbitraryBS1
