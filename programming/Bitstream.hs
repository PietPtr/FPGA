{-# LANGUAGE RecordWildCards #-}
module Bitstream where

import Clash.Prelude
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)

import ConfigJSON
import ManualDesign

import Debug.Trace

type Bitstream = [BitVector 1]

ei :: (Enum a) => a -> BitVector 1
ei = fromIntegral . fromEnum

vl :: BitVector 8 -> Bitstream
vl bv = L.map (\i -> ei $ bv ! i) [7,6..0]

data FPGADef = FPGADef {
        size :: Int
    }

instance Default FPGADef where
    def = FPGADef {
            size = 4
        }

class Bitstreamable a where
    bitstream :: FPGADef -> a -> Bitstream

printAligned :: Bitstream -> IO ()
printAligned stream = mapM_ print $ chunksOf 10 stream

reverseConcat :: [a] -> [a] -> [a]
reverseConcat a b = b L.++ a

instance Bitstreamable FPGAConfig where
    bitstream defs conf = L.foldl reverseConcat [] $ 
        L.map (bitstream defs) (L.map ((Map.!) tiles) layout)
        where
            FPGADef{..} = defs
            FPGAConfig{..} = complete
            complete = completeFPGAConfig defs conf

            -- Layout of device, the tile at index zero is where the bitstream enters
            -- the tilegrid, hence the bitstream will have the config for that tile
            -- _last_.
            layout :: [FPGACoordinate]
            layout = [FPGACoordinate x y | y <- [0..(size-1)], x <- [0..(size-1)]]

-- an FPGAConfig is not guaranteed to be complete: not every tile
-- will have a configuration. A complete FPGAConfig must be constructed
completeFPGAConfig :: FPGADef -> FPGAConfig -> FPGAConfig
completeFPGAConfig defs conf = FPGAConfig $ constructCompleteFPGA rows
    where
        FPGADef{..} = defs
        FPGAConfig{..} = conf
        
        row y = L.map (\x -> getOrDef $ FPGACoordinate x y) [0..(size-1)]
        rows = L.map row [0..(size-1)]

        getOrDef coord = case Map.lookup coord tiles of
            Nothing -> def
            Just tileConf -> tileConf

        constructCompleteFPGA rowConfs = L.foldl Map.union Map.empty rowList
            where
                rowList = L.map (uncurry constructCompleteRow) $ L.zip rowConfs [0..]

        constructCompleteRow confs y = L.foldl (go y) Map.empty (L.zip confs [0..])
            where
                go y map (conf, x) = Map.insert (FPGACoordinate x y) conf map


instance Bitstreamable [TileConfig] where
    -- confs is a list where the first element is the leftmost tile
    -- i.e. the indexing of this list matches the x coordinate in the FPGA grid
    bitstream d confs = L.foldr go [] confs
        where go conf stream = stream L.++ bitstream d conf

instance Bitstreamable TileConfig where
    bitstream d conf = bitstream d vertLut L.++ bitstream d horzLut
        where
            TileConfig{..} = conf

instance Bitstreamable LUTConfig where
    bitstream _ conf = ei enableReg1 : ei enableReg0 : vl lutConfig
        where
            LUTConfig{..} = conf
