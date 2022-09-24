module Top where

import Clash.Prelude hiding (Bit, or)

type Bit = BitVector 1

type LutSelect = BitVector 2
type LutOut = BitVector 1
type Table = BitVector 4

lut :: HiddenClockResetEnable dom =>
    Signal dom Bool -> Signal dom Bit -> Signal dom (BitVector 2) -> 
        (Signal dom Bit, Signal dom Bit)
lut flashing config selector = (lutOut, regmuxconf) 
    where
        lutOut = mux4to1 <$> selector <*> lut00conf <*> lut01conf <*> lut10conf <*> lut11conf 

        lut00conf = configReg flashing config
        lut01conf = configReg flashing lut00conf
        lut10conf = configReg flashing lut01conf
        lut11conf = configReg flashing lut10conf
        regmuxconf = configReg flashing lut11conf

        mux4to1 sel a b c d = case sel of
            0b00 -> a
            0b01 -> b
            0b10 -> c
            0b11 -> d


configReg :: HiddenClockResetEnable dom =>
    Signal dom Bool -> Signal dom Bit -> Signal dom Bit
configReg flashing config = dout
    where
        dout = register 0b0 new
        new = mux flashing dout config


type Tight = BitVector 2
type Wide = BitVector 3

data TileInputs = TileInputs {
        up_in :: Wide, -- -> up_out
        left_in :: Wide, -- -> left_out
        down_in :: Tight, -- -> down_out
        right_in :: Tight -- -> right_out
    } deriving Show

data TileInputRow (n :: Nat) = TileInputRow {
        upr_in :: Vec n Wide,
        leftr_in :: Wide,
        downr_in :: Vec n Tight,
        rightr_in :: Tight
    }

instance Default TileInputs where
    def = TileInputs {
        up_in = 0b000,
        left_in = 0b000,
        down_in = 0b00,
        right_in = 0b00
    }

data TileOutputs = TileOutputs {
        up_out :: Wide,
        left_out :: Wide,
        down_out :: Tight,
        right_out :: Tight
    }

data TileOutputRow (n :: Nat) = TileOutputRow {
        upr_out :: Vec n Wide,
        leftr_out :: Wide,
        downr_out :: Vec n Tight,
        rightr_out :: Tight
    }


-- row :: (HiddenClockResetEnable dom, KnownNat r) =>
--     Signal dom Bool -> Signal dom Bit -> 
--     Signal dom (TileInputRow r) -> 
--     Signal dom (TileOutputRow r)
-- row flashing config row = undefined
--     where
--         tiles = scanl holeja (tile flashing config) (upr_in <$> row)

manualRow flashing config row = (t1, t2conf)
    where
        (t1, t1conf) = tile flashing config (mkTileInput <$> row <*> 0 <*> (leftr_in <$> row) <*> (right_out <$> t2))
        (t2, t2conf) = tile flashing t1conf (mkTileInput <$> row <*> 1 <*> (left_out <$> t1) <*> (right_out <$> t3))
        (t3, t3conf) = tile flashing t2conf (mkTileInput <$> row <*> 2 <*> (left_out <$> t2) <*> (rightr_out <$> row))

        mkTileInput row idx left right = TileInputs {
                up_in = (upr_in row) !! idx,
                left_in = left,
                down_in = (downr_in row) !! idx,
                right_in = right
            }

or :: Bit -> Bit -> Bit
or 0 0 = 0
or _ _ = 1

get :: BitPack a => Int -> a -> Bit
get idx vec = fromIntegral $ vec ! idx

sconcat :: (Applicative f, KnownNat m) => 
    f (BitVector n) -> f (BitVector m) -> f (BitVector (n + m))
sconcat = liftA2 (++#)

tile :: HiddenClockResetEnable dom =>
    Signal dom Bool -> Signal dom Bit -> Signal dom TileInputs -> 
    (Signal dom TileOutputs, Signal dom Bit)
tile flashing config inp = (TileOutputs <$> up_out <*> left_out <*> down_out <*> right_out, r2conf)
    where
        up_out    = up1 `sconcat` up2 `sconcat` up2
        left_out  = left1 `sconcat` left2 `sconcat` left2
        down_out  = down2 `sconcat` down1 
        right_out = right2 `sconcat` right2

        up_in0 = get 0 <$> (up_in <$> inp)
        up_in1 = get 1 <$> (up_in <$> inp)
        up_in2 = get 2 <$> (up_in <$> inp)

        left_in0 = get 0 <$> (left_in <$> inp)
        left_in1 = get 1 <$> (left_in <$> inp)
        left_in2 = get 2 <$> (left_in <$> inp)

        down_in0 = get 0 <$> (down_in <$> inp)
        down_in1 = get 1 <$> (down_in <$> inp)
        
        right_in0 = get 0 <$> (right_in <$> inp)
        right_in1 = get 1 <$> (right_in <$> inp)

        (up1, u1conf)    = lut flashing d2conf (up_in0             `sconcat` up_in1)
        (up2, u2conf)    = lut flashing u1conf ((up1 +|+ left1)    `sconcat` up_in2)
        (left1, l1conf)  = lut flashing u2conf (left_in0           `sconcat` left_in1)
        (left2, l2conf)  = lut flashing l1conf ((left1 +|+ up1)    `sconcat` left_in2) 
        (down1, d1conf)  = lut flashing config (down_in0           `sconcat` down_in1)
        (down2, d2conf)  = lut flashing d1conf ((down1 +|+ left2)  `sconcat` down_in0)
        (right1, r1conf) = lut flashing l2conf (right_in0          `sconcat` right_in1)
        (right2, r2conf) = lut flashing r1conf ((right1 +|+ up2)   `sconcat` right_in0)

        (+|+) = liftA2 or



