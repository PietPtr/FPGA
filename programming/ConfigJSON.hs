{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ConfigJSON where

import Clash.Prelude hiding (pack, map, filter, foldl, (++))

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text hiding (filter)
import Data.List

import GHC.Generics

{-
QUICK FACTS
- LSB is the last element of the string
- format is thus qpqpqpqp
- b++#a is the selector, hence the truth table is in "reverse"
So the bitstring 87654321 encodes the table:
b a | q p
0 0 | 2 1
0 1 | 4 3
1 0 | 6 5
1 1 | 8 7

-}




instance ToJSON (BitVector 8) where
    toJSON bv = String $ pack $ (filter (/= '_') $ show bv)

data LUTConfig = LUTConfig {
        lutConfig :: BitVector 8,
        enableReg0 :: Bool,
        enableReg1 :: Bool
} deriving (Generic, ToJSON, Show)

instance Default LUTConfig where
    def = LUTConfig 0 True True

data TileConfig = TileConfig {
        horzLut :: LUTConfig,
        vertLut :: LUTConfig
    } deriving (Show, Generic)

instance Default TileConfig where
    def = TileConfig def def

instance ToJSON TileConfig where
    toEncoding = genericToEncoding defaultOptions

data FPGACoordinate = FPGACoordinate {
        x :: Int ,
        y :: Int
    } deriving (Generic, Ord, Eq)

instance Show FPGACoordinate where
    show (FPGACoordinate x y) = show x ++ "," ++ show y

instance ToJSON FPGACoordinate where
    toEncoding = genericToEncoding defaultOptions

instance ToJSONKey FPGACoordinate where
    toJSONKey = toJSONKeyText (pack . show)


data FPGAConfig = FPGAConfig {
        tiles :: Map FPGACoordinate TileConfig
    } deriving (Show, Generic)

instance ToJSON FPGAConfig where
    toEncoding = genericToEncoding defaultOptions
