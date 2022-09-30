{-# LANGUAGE RecordWildCards #-}
module Bitstream where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)

import ConfigJSON
import ManualDesign

import Debug.Trace

ei :: (Enum a) => a -> BitVector 1
ei = fromIntegral . fromEnum

vl :: BitVector 8 -> [BitVector 1]
vl bv = L.map (\i -> ei $ bv ! i) [7,6..0]

data FPGADef = FPGADef {
        size :: Int
    }

instance Default FPGADef where
    def = FPGADef {
            size = 4
        }

class BitStream a where
    bitstream :: FPGADef -> a -> [BitVector 1]

instance BitStream FPGAConfig where
    bitstream defs conf = trace (show $ row 0) undefined
        where
            FPGADef{..} = defs
            FPGAConfig{..} = conf
            -- build list of rows of tiles, filling in the gaps in the conf
            -- make BitStream instance for those rows
            -- connect the rows here again
            row y = L.map (\x -> getOrDef $ FPGACoordinate x y) [0..(size-1)]

            getOrDef coord = case Map.lookup coord tiles of
                Nothing -> def
                Just tileConf -> tileConf

instance BitStream [TileConfig] where
    -- confs is a list where the first element is the leftmost tile
    -- i.e. the indexing of this list matches the x coordinate in the FPGA grid
    bitstream d confs = L.foldr go [] confs
        where go conf stream = stream L.++ bitstream d conf

instance BitStream TileConfig where
    bitstream d conf = bitstream d vertLut L.++ bitstream d horzLut
        where
            TileConfig{..} = conf

instance BitStream LUTConfig where
    bitstream _ conf = ei enableReg1 : ei enableReg0 : vl lutConfig
        where
            LUTConfig{..} = conf
