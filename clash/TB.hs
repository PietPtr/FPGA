{-# LANGUAGE RecordWildCards #-}
module TB where

import Clash.Prelude hiding (Bit)

import Lut2
import FPGAs

import qualified Data.List as L

data LUTConfig = LUTConfig {
        lut00 :: Vec 2 Bit,
        lut01 :: Vec 2 Bit,
        lut10 :: Vec 2 Bit,
        lut11 :: Vec 2 Bit,
        enReg0 :: Bool,
        enReg1 :: Bool
    } deriving (Generic, Show, NFDataX)

lc_halfAdder = LUTConfig {
        lut00 = 0:>0:>Nil,
        lut01 = 1:>0:>Nil,
        lut10 = 1:>0:>Nil,
        lut11 = 0:>1:>Nil,
        enReg0 = False,
        enReg1 = False
    }

bitstream :: LUTConfig -> [Maybe Bit]
bitstream conf = L.map Just [
    fromIntegral $ fromEnum enReg1, 
    fromIntegral $ fromEnum enReg0, 
    lut11 !! 1, lut11 !! 0,
    lut10 !! 1, lut10 !! 0,
    lut01 !! 1, lut01 !! 0,
    lut00 !! 1, lut00 !! 0]
    where
        LUTConfig{..} = conf

simmable :: (Functor f) => (f a -> f b -> f c) -> (f (a, b) -> f c)
simmable f tup = f (fst <$> tup) (snd <$> tup)

simmableLut tup = bundle $ lut config sel0 sel1
    where (config, sel0, sel1) = unbundle tup

simConfigReg = simulate @System (configReg (0b0 :: BitVector 10)) config
    where
        config = bitstream $ lc_halfAdder

simLut = mapM_ print $ L.take 19 $ simulate @System simmableLut (L.zip3 config sel1 sel0)
    where
        config = bitstream lc_halfAdder L.++ L.repeat Nothing
        sel0 = (L.take 20 $ L.repeat 0)
        sel1 = (L.take 20 $ L.repeat 0)

simLutConfigProp = mapM_ print $ L.take 22 $ simulate @System simmableLut (L.zip3 config sel0 sel0)
    where
        config = L.map Just $ cycle [0,1]
        sel0 = L.repeat 0

loadOnes = simulate @System (simmable manualFPGA) oneStream

oneStream :: (KnownNat n) => [(Maybe Bit, FPGAInput n)]
oneStream = L.zip (L.take 160 $ L.repeat $ Just 1) (L.repeat def)


dataLoop :: HiddenClockResetEnable dom =>
    Signal dom Bool -> Signal dom Bool
dataLoop break = out
    where
        out = not <$> muxed
        reg = register False out
        muxed = mux break out reg

-- should output [True, False, True, False,<hang>
simLoop = simulate @System dataLoop [False, False, False, False, True, True, True]

topEntity ::
    Clock System
    -> Reset System
    -> Enable System
    -> Signal System Bool
    -> Signal System Bool
topEntity = exposeClockResetEnable dataLoop