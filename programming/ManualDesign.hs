module ManualDesign where

import ConfigJSON
import Clash.Prelude hiding (pack, unpack, map, filter, foldl, (++), Bit)
import qualified Clash.Prelude as Clash

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

import Data.Aeson

-------------
-- Designs --
-------------

boringConfig = FPGAConfig $ Map.fromList [
        (FPGACoordinate 0 0, TileConfig (LUTConfig 0 False True) (LUTConfig 1 True False)),
        (FPGACoordinate 0 1, TileConfig (LUTConfig 7 False True) (LUTConfig 0xaa False False))
    ]

invertedTurnaround = toLut $ \(a, b) -> (not a, not b)

turnAround = toLut @Bit id
forward_a = toLut @Bit $ \(a, b) -> (0, a)
forward_b = toLut @Bit $ \(a, b) -> (b, 0)

clockConfig = FPGAConfig $ Map.fromList [
        (FPGACoordinate 0 0, TileConfig {
            horzLut = def,
            vertLut = LUTConfig turnAround False False
        }),
        (FPGACoordinate 1 0, TileConfig {
            horzLut = LUTConfig forward_b False False,
            vertLut = LUTConfig turnAround True False
        })
    ]

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
    putStrLn "ab | pq"
    putStrLn $ "00 | " ++ show l00
    putStrLn $ "10 | " ++ show l10
    putStrLn $ "01 | " ++ show l01
    putStrLn $ "11 | " ++ show l11
    where
        l00 = slice d1 d0 bv
        l01 = slice d3 d2 bv
        l10 = slice d5 d4 bv
        l11 = slice d7 d6 bv