{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bitcoin.CompactFilter
   ( BlockFilter
   , blockFilter
   , BlockFilterHeader
   , blockFilterHeader
   , filterHeaderToHex
   , filterHeaderFromHex
   , genesisHeader
   , filterContents
   , encodeFilter
   , isMember
   ) where

import           Control.Monad                    (replicateM, (>=>))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import qualified Control.Monad.Trans.State.Strict as St
import           Data.Bits                        (shiftL, shiftR, testBit)
import           Data.Bitstream                   (Bitstream, Right)
import qualified Data.Bitstream                   as BiS
import           Data.Bool                        (bool)
import           Data.ByteArray.Hash              (SipHash (..), SipKey (..),
                                                   sipHash)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Foldable                    (foldl')
import           Data.List                        (sort)
import           Data.Serialize                   (Get, Serialize (..), decode,
                                                   encode, getWord16le,
                                                   getWord32le, getWord64le,
                                                   getWord8, putByteString,
                                                   putWord16le, putWord32le,
                                                   putWord64le, putWord8,
                                                   runGet)
import           Data.Text                        (Text)
import           Data.Word                        (Word64, Word8)
import           Haskoin.Block                    (Block, BlockHash,
                                                   blockHeader, blockTxns,
                                                   headerHash)
import           Haskoin.Crypto                   (Hash256, doubleSHA256)
import           Haskoin.Script                   (Script (..), ScriptOp (..))
import           Haskoin.Transaction              (scriptOutput, txOut)
import           Haskoin.Util                     (decodeHex, encodeHex)


paramP :: Int
paramP = 19


paramM :: Word64
paramM = 784931


-- | Hashes of scripts in the block
newtype BlockFilter = BlockFilter { blockFilter :: [Word64] } deriving (Eq, Show)


blockFilterSize :: BlockFilter -> CompactSize
blockFilterSize = CompactSize . length . blockFilter


newtype BlockFilterHeader = BlockFilterHeader { getBFHeader :: Hash256 }
    deriving (Eq, Show, Ord, Serialize)


filterHeaderToHex :: BlockFilterHeader -> Text
filterHeaderToHex = encodeHex . BS.reverse . encode . getBFHeader


filterHeaderFromHex :: Text -> Either String BlockFilterHeader
filterHeaderFromHex = maybe (Left "Invalid hex") Right . decodeHex >=> decode . BS.reverse


genesisHeader :: BlockFilterHeader
Right genesisHeader = BlockFilterHeader <$> decode (BS.replicate 32 0x0)


blockFilterHeader :: BlockFilterHeader -> BlockFilter -> BlockFilterHeader
blockFilterHeader prev bf
    = BlockFilterHeader . doubleSHA256
    $ (encode . doubleSHA256) (encode bf) <> encode (getBFHeader prev)


instance Serialize BlockFilter where
    put bf = put (blockFilterSize bf) >> putByteString (constructGCS paramP $ blockFilter bf)
    get = get >>= fmap BlockFilter . getGolombRiceSet paramP . unCompactSize


-- | Scripts in the block which belong in the BIP158 block filter
filterContents
    :: [ByteString]
    -- ^ previous output scripts spent in this block
    -> Block
    -> [ByteString]
filterContents prev b = filter scriptFilter prev <> these
    where
    these            = filter scriptFilter . fmap scriptOutput $ blockTxns b >>= txOut
    scriptFilter scr = not (BS.null scr) && contentFilter scr

    contentFilter bs = case decode bs of
        Right (Script (OP_RETURN : _)) -> False
        _                              -> True


-- | Construct a BIP158 filter from a block
encodeFilter :: [ByteString] -> Block -> BlockFilter
encodeFilter os b = BlockFilter s
    where
    h  = headerHash $ blockHeader b
    bs = toSet $ filterContents os b
    s  = hashedSetConstruct (sipKey h) paramM (length bs) bs


-- | Test membership
isMember :: BlockHash -> [ByteString] -> BlockFilter -> Bool
isMember h bs (BlockFilter bf) = orderedScan hs bf
    where
    k  = sipKey h
    hs = hashedSetConstruct k paramM (length bf) bs


orderedScan :: [Word64] -> [Word64] -> Bool
orderedScan xs@(x : xs') hs@(h : hs')
    | x > h     = orderedScan xs hs'
    | x < h     = orderedScan xs' hs
    | otherwise = True
orderedScan _ _ = False


type GetBits = StateT [Bool] Get


getGolombRiceSet :: Int -> Int -> Get [Word64]
getGolombRiceSet p n = fmap unDiffs . evalBitstream $ replicateM n getEncoded
    where
    getEncoded = do
        q <- unaryPart
        r <- fromBits <$> getBits p
        return $ q * 2^p + r


unaryPart :: GetBits Word64
unaryPart = go 0
    where
    go q   = getBit >>= bool (return q) (go $ q + 1)
    getBit = head <$> getBits 1


evalBitstream :: GetBits a -> Get a
evalBitstream = (`evalStateT` mempty)


getBits :: Int -> GetBits [Bool]
getBits n = do
    bs <- St.get
    let l = length bs
        (q, r) = (n - l) `quotRem` 8

        combine m t = bs <> mconcat m <> t

        lastByte
            | r > 0     = getBs >>= takeSome r
            | otherwise = mempty <$ St.put mempty

        getBs         = byteBits <$> lift getWord8
        takeSome m xs = take m xs <$ St.put (drop m xs)

    if n < l
        then takeSome n bs
        else combine <$> replicateM q getBs <*> lastByte


sipKey :: BlockHash -> SipKey
sipKey h = SipKey k1 k2
    where
    Right (k1, k2) = runGet word64Pair $ encode h
    word64Pair     = (,) <$> getWord64le <*> getWord64le


hashToRange :: Word64 -> SipKey -> ByteString -> Word64
hashToRange f k bs = v
    where
    SipHash h = sipHash k bs
    v         = remap (fromIntegral f) (fromIntegral h)

    remap :: Integer -> Integer -> Word64
    remap x y = fromIntegral $ (x * y) `shiftR` 64


hashedSetConstruct :: SipKey -> Word64 -> Int -> [ByteString] -> [Word64]
hashedSetConstruct k m n bs = toSet $ hashToRange f k <$> bs
    where
    f = fromIntegral n * m


toSet :: Ord a => [a] -> [a]
toSet = dedup . sort
    where
    dedup = \case
        (x0 : xs@(x1 : _))
            | x0 == x1  -> dedup xs
            | otherwise -> x0 : dedup xs
        xs -> xs


constructGCS
    :: Int
    -- ^ modulus
    -> [Word64]
    -- ^ sorted list of input values
    -> ByteString
constructGCS p
    = BiS.toByteString
    . foldMap (golombRiceEncode p)
    . diffs


diffs :: Num a => [a] -> [a]
diffs xs = zipWith (-) xs (0 : xs)


unDiffs :: Num a => [a] -> [a]
unDiffs (x : xs) = scanl (+) x xs
unDiffs []       = []


golombRiceEncode :: Int -> Word64 -> Bitstream Right
golombRiceEncode p v = x <> BiS.singleton False <> y
    where
    q = fromIntegral $ v `shiftR` p
    x = BiS.replicate q True
    y = BiS.fromNBits p v


fromBits :: Num a => [Bool] -> a
fromBits = foldl' onBit 0
    where
    onBit n b = 2 * n + bool 0 1 b


byteBits :: Word8 -> [Bool]
byteBits b = testBit b <$> reverse [0 .. 7]


newtype CompactSize = CompactSize { unCompactSize :: Int } deriving (Eq, Ord, Enum, Num, Real, Integral)


instance Serialize CompactSize where
    get = CompactSize <$> (getWord8 >>= getCS)
        where
        getCS s
            | s < 253   = return $ fromIntegral s
            | s == 253  = fromIntegral <$> getWord16le
            | s == 254  = fromIntegral <$> getWord32le
            | otherwise = fromIntegral <$> getWord64le

    put (CompactSize n)
        | n < 0       = error $ "Invalid CompactSize: " <> show n
        | n < 253     = putWord8 (fromIntegral n)
        | n < bound16 = putWord8 253 >> putWord16le (fromIntegral n)
        | n < bound32 = putWord8 254 >> putWord32le (fromIntegral n)
        | otherwise   = putWord8 255 >> putWord64le (fromIntegral n)
        where
        bound16 = 1 `shiftL` 16
        bound32 = 1 `shiftL` 32
