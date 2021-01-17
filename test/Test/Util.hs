{-# LANGUAGE OverloadedStrings #-}

module Test.Util (
    Example (..),
    decodeFromHex,
    parseHex,
) where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize, decode)
import Data.Text (Text)
import Haskoin.Block (Block, BlockHash, BlockHeight)
import qualified Haskoin.Util as U

import Bitcoin.CompactFilter (
    BlockFilter,
    BlockFilterHeader,
    filterHeaderFromHex,
 )

decodeFromHex :: Serialize a => Text -> Either String a
decodeFromHex = parseHex >=> decode

parseHex :: Text -> Either String ByteString
parseHex = maybe (Left "Bad hex") Right . U.decodeHex

data Example = Example
    { exampleHeight :: BlockHeight
    , exampleBlockHash :: BlockHash
    , exampleBlock :: Block
    , examplePrevOuts :: [ByteString]
    , exampleFilter :: BlockFilter
    , examplePrevHeader :: BlockFilterHeader
    , exampleHeader :: BlockFilterHeader
    }
    deriving (Eq, Show)

instance FromJSON Example where
    parseJSON = withObject "Example" $ \obj ->
        Example
            <$> obj .: "height"
            <*> (fromHex =<< obj .: "block_hash")
            <*> (fromHex =<< obj .: "block")
            <*> (either fail return . traverse parseHex =<< obj .: "prev_outs")
            <*> (fromHex =<< obj .: "filter")
            <*> (either fail return . filterHeaderFromHex =<< obj .: "prev_filter_header")
            <*> (either fail return . filterHeaderFromHex =<< obj .: "filter_header")

fromHex :: Serialize a => Text -> Parser a
fromHex = either fail return . decodeFromHex
