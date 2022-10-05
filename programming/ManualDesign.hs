module ManualDesign where

import ConfigJSON
import Clash.Prelude hiding (pack, unpack, map, filter, foldl, (++), Bit)
import qualified Clash.Prelude as Clash

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

import Data.Aeson

import Bitstream

-------------
-- Designs --
-------------

boringConfig = FPGAConfig $ Map.fromList [
        (FPGACoordinate 0 0, TileConfig (LUTConfig 0 False True) (LUTConfig 1 True False)),
        (FPGACoordinate 0 1, TileConfig (LUTConfig 7 False True) (LUTConfig 0xaa False False))
    ]

invertedTurnaround = toLut $ \(a, b) -> (not a, not b)

-- (a, b) -> (p, q)
turnAround = toLut @Bit id
forward_a = toLut @Bit $ \(a, _) -> (0, a)
forward_b = toLut @Bit $ \(_, b) -> (b, 0)
forward = toLut @Bit $ \(a, b) -> (b, a)
p_is_not_a = toLut $ \(a, _) -> (not a, False)
q_is_not_b = toLut $ \(_, b) -> (False, not b)
p_is_a = toLut @Bit $ \(a, _) -> (a, 0)
q_is_b = toLut @Bit $ \(_, b) -> (0, b)

clockConfig = FPGAConfig $ Map.fromList [
        (FPGACoordinate 0 0, TileConfig {
            horzLut = def,
            vertLut = LUTConfig q_is_not_b False False
        }),
        (FPGACoordinate 1 0, TileConfig {
            horzLut = LUTConfig forward_b False False,
            vertLut = LUTConfig p_is_a True False
        })
    ]

-- Generate a bitstream: printAligned $ bitstream def clockConfig

-----------
-- Types --
-----------

type Bit = BitVector 1

-------------
-- Helpers --
-------------

toLut :: forall a . (Enum a, BitPack a) => ((a, a) -> (a, a)) -> BitVector 8
toLut f = 
    (conv $ f (toEnum 1, toEnum 1)) ++#
    (conv $ f (toEnum 1, toEnum 0)) ++#
    (conv $ f (toEnum 0, toEnum 1)) ++#
    (conv $ f (toEnum 0, toEnum 0))
    where
        forceBit :: a -> BitVector 1
        forceBit p = fromIntegral $ (Clash.pack p) ! 0

        conv (p, q) = forceBit p ++# forceBit q

pretty :: BitVector 8 -> IO ()
pretty bv = do
    putStrLn "ba | qp"
    putStrLn $ "00 | " ++ show l00
    putStrLn $ "01 | " ++ show l01
    putStrLn $ "10 | " ++ show l10
    putStrLn $ "11 | " ++ show l11
    where
        l00 = slice d1 d0 bv
        l01 = slice d3 d2 bv
        l10 = slice d5 d4 bv
        l11 = slice d7 d6 bv