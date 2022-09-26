{-# LANGUAGE OverloadedStrings #-}
module Lut2 where

import Clash.Prelude hiding (Bit, or)

import Data.Maybe
import Debug.Trace
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Bit = BitVector 1

type LutSelect = BitVector 2
type LutOut = BitVector 1
type Table = BitVector 4
data Dir = Horz | Vert
data Location = Location Int Int Dir

instance Show Dir where
    show Horz = "h"
    show Vert = "v"

instance Show Location where
    show (Location x y dir) = show x L.++ "_" L.++ show y L.++ "_" L.++ show dir

routeFlash :: Maybe b -> b -> Maybe b
routeFlash flash val = (const val) <$> flash

get :: (Show i, Enum i, KnownNat n) => i -> BitVector n -> BitVector 1
get idx vec = -- trace (show vec L.++ " ! " L.++ show idx L.++ " = " L.++ show result) result
    result
    where
        result = fromIntegral $ vec ! idx


lut :: HiddenClockResetEnable dom => Location ->
    Signal dom (Maybe Bit) -> Signal dom Bit -> Signal dom Bit ->
    (Signal dom Bit, Signal dom Bit, Signal dom (Maybe Bit))
lut loc flash sel1 sel0 = (out0, out1, outflash)
    where
        out0 = outmux <$> flash <*> reg0muxconf <*> reg0Out <*> lut0Out
        out1 = outmux <$> flash <*> reg1muxconf <*> reg1Out <*> lut1Out
        outmux flash condition thenSig elseSig = case (condition, flash) of
                (_, Just _) -> 0
                (1, Nothing) -> thenSig
                (0, Nothing) -> elseSig
        
        outflash = routeFlash <$> flash <*> reg1muxconf

        selector = (++#) <$> sel1 <*> sel0

        lut0Out = get <$> (evenSelector <$> selector)        <*> configState
        lut1Out = get <$> ((\s -> evenSelector s + 1) <$> selector) <*> configState

        evenSelector :: BitVector 2 -> BitVector 3
        evenSelector selector = 2 * (resize selector)

        reg0Out = register (0 :: Bit) lut0Out
        reg1Out = register (0 :: Bit) lut1Out

        configState = traceSignal1 confName $ configReg (0b0000000000 :: BitVector 10) flash
        confName = "conf_" L.++ show loc

        reg0muxconf = get <$> 8 <*> configState
        reg1muxconf = get <$> 9 <*> configState

traceReg :: (Show a, NFDataX a, HiddenClockResetEnable dom) =>
    String -> a -> Signal dom a -> Signal dom a
traceReg name initial = mealy machine initial
    where
        machine state inp = (trace (name L.++ " " L.++ show inp) inp, state)

configReg :: (HiddenClockResetEnable dom, KnownNat n) =>
    (BitVector n) -> Signal dom (Maybe Bit) -> Signal dom (BitVector n)
configReg initial flash = dout
    where
        dout = register initial new
        new = shiftIf <$> flash <*> dout

        shiftIf f old = case f of
            Just bit -> (shiftL old 1) .|. (resize bit)
            Nothing -> old

data TileInputs = TileInputs {
        up_in :: Bit, -- -> up_out
        left_in :: Bit, -- -> left_out
        down_in :: Bit, -- -> down_out
        right_in :: Bit -- -> right_out
    } deriving (NFDataX, Generic, Show, BitPack)

data TileInputRow (n :: Nat) = TileInputRow {
        upr_in :: Vec n Bit,
        leftr_in :: Bit,
        downr_in :: Vec n Bit,
        rightr_in :: Bit
    } deriving (NFDataX, Generic, Show, BitPack)

instance Default TileInputs where
    def = TileInputs {
        up_in = 0b0,
        left_in = 0b0,
        down_in = 0b0,
        right_in = 0b0
    }

data TileOutputs = TileOutputs {
        up_out :: Bit,
        left_out :: Bit,
        down_out :: Bit,
        right_out :: Bit
    } deriving (NFDataX, Generic, Show, BitPack)

data TileOutputRow (n :: Nat) = TileOutputRow {
        upr_out :: Vec n Bit,
        leftr_out :: Bit,
        downr_out :: Vec n Bit,
        rightr_out :: Bit
    } deriving (NFDataX, Generic, Show, BitPack)

data FPGAInput (n :: Nat) = FPGAInput {
        upf_in :: Vec n Bit,
        leftf_in :: Vec n Bit,
        downf_in :: Vec n Bit,
        rightf_in :: Vec n Bit
    } deriving (NFDataX, Generic, Show, BitPack)

data FPGAOutput (n :: Nat) = FPGAOutput {
        upf_out :: Vec n Bit,
        leftf_out :: Vec n Bit,
        downf_out :: Vec n Bit,
        rightf_out :: Vec n Bit
    } deriving (NFDataX, Generic, Show, BitPack)

instance (KnownNat n) => Default (FPGAInput n) where
    def = FPGAInput {
        upf_in = repeat 0,
        leftf_in = repeat 0,
        downf_in = repeat 0,
        rightf_in = repeat 0
    }

main :: IO ()
main = do
    let configSig = pure $ Just 1
    let topOut = topEntity systemClockGen systemResetGen enableGen configSig (pure def)
    vcd <- dumpVCD (0, 203) topOut ["conf_0_3_v"]
    case vcd of
        Left msg -> error msg
        Right contents -> TIO.writeFile "fpga.vcd" contents

topEntity ::
    Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe Bit)
    -> Signal System (FPGAInput 4)
    -> Signal System (FPGAOutput 4)
topEntity = exposeClockResetEnable manualFPGA

manualFPGA :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Bit) -> Signal dom (FPGAInput 4) -> Signal dom (FPGAOutput 4)
manualFPGA flash inp = out <$> rows
    where
        (r1, r1conf) = manualRow 0 flash  $ mkRowInput <$> inp <*> 0 <*> (downr_out <$> r2) <*> (downf_in <$> inp)
        (r2, r2conf) = manualRow 1 r1conf $ mkRowInput <$> inp <*> 1 <*> (downr_out <$> r3) <*> (upr_out <$> r1)
        (r3, r3conf) = manualRow 2 r2conf $ mkRowInput <$> inp <*> 2 <*> (downr_out <$> r4) <*> (upr_out <$> r2)
        (r4, r4conf) = manualRow 3 r3conf $ mkRowInput <$> inp <*> 3 <*> (downf_in <$> inp) <*> (upr_out <$> r3)
        rows = bundle (r1:>r2:>r3:>r4:>Nil)

        mkRowInput inp idx up down = TileInputRow {
                upr_in = up,
                leftr_in = (leftf_in inp) !! idx,
                downr_in = down,
                rightr_in = (rightf_in inp) !! idx
            }

        out rows = FPGAOutput {
            upf_out = upr_out (last rows),
            leftf_out = map leftr_out rows,
            downf_out = downr_out (head rows),
            rightf_out = map rightr_out rows
        }

-- TODO: Template haskell misschien dan maar gewoon???
manualRow rowNum flash row = (out <$> tiles, t2conf)
    where
        (t1, t1conf) = tile (0, rowNum) flash  (mkTileInput <$> row <*> 0 <*> (left_out <$> t2) <*> (leftr_in <$> row))
        (t2, t2conf) = tile (1, rowNum) t1conf (mkTileInput <$> row <*> 1 <*> (left_out <$> t3) <*> (right_out <$> t1))
        (t3, t3conf) = tile (2, rowNum) t2conf (mkTileInput <$> row <*> 2 <*> (left_out <$> t4) <*> (right_out <$> t2))
        (t4, t4conf) = tile (3, rowNum) t3conf (mkTileInput <$> row <*> 3 <*> (rightr_in <$> row) <*> (right_out <$> t3))
        tiles = bundle (t1:>t2:>t3:>t4:>Nil)

        mkTileInput row idx left right = TileInputs {
                up_in = (upr_in row) !! idx,
                left_in = left,
                down_in = (downr_in row) !! idx,
                right_in = right
            }

        out tiles = TileOutputRow {
                upr_out = map up_out tiles,
                leftr_out = left_out (head tiles),
                downr_out = map down_out tiles,
                rightr_out = right_out (last tiles)
            }

or :: Bit -> Bit -> Bit
or 0 0 = 0
or _ _ = 1

tile :: HiddenClockResetEnable dom => (Int, Int) ->
    Signal dom (Maybe Bit) -> Signal dom TileInputs -> 
    (Signal dom TileOutputs, Signal dom (Maybe Bit))
tile (x, y) flash inp = (TileOutputs <$> up_out <*> left_out <*> down_out <*> right_out, vertconf)
    where
        up_out = up
        left_out = left
        down_out = down +|+ l_in
        right_out = right +|+ d_in

        u_in = up_in <$> inp
        l_in = left_in <$> inp
        d_in = down_in <$> inp
        r_in = right_in <$> inp

        (left, right, horzconf) = lut (Location x y Horz) flash r_in (l_in +|+ down)  
        (up, down, vertconf)    = lut (Location x y Vert) horzconf u_in (d_in +|+ right)

        (+|+) = liftA2 or

