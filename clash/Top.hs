module Top where

import Clash.Prelude hiding (Bit, or)

type Bit = BitVector 1

type LutSelect = BitVector 2
type LutOut = BitVector 1
type Table = BitVector 4

-- TODO: programmability?
lut :: HiddenClockResetEnable dom =>
    Signal dom Bool -> Signal dom Bit -> Signal dom (BitVector 2) -> 
        Signal dom (BitVector 1, BitVector 1)
lut flashing config selector = bundle (lutOut, configOut) 
    where
        lutOut = (\table sel -> fromIntegral $ table ! sel) <$> state <*> selector
        state' = id state -- TODO: hier programmability somehow?, shift regje toolen?
        state = register config state'

        lut00conf = configReg flashing config
        lut01conf = configReg flashing lut00conf
        lut10conf = configReg flashing lut01conf
        lut11conf = configReg flashing lut10conf
        regmuxconf = configReg flashing lut11conf

configReg :: HiddenClockResetEnable dom =>
    Signal dom Bool -> Signal dom Bit -> Signal dom Bit
configReg flashing config = dout
    where
        dout = register 0b0 new
        new = mux flashing dout config

halfAdder :: HiddenClockResetEnable dom =>
    Signal dom (Bit, Bit) -> Signal dom (BitVector 2)
halfAdder inp = bundle $ ((++#) <$> andGate select <*> xorGate select)
    where
        (a, b) = unbundle inp
        select = (++#) <$> a <*> b
        andGate = lut 0b1000
        xorGate = lut 0b0110

type Tight = BitVector 2
type Wide = BitVector 3

data TileInputs = TileInputs {
        up_in :: Wide, -- -> up_out
        left_in :: Wide, -- -> left_out
        down_in :: Tight, -- -> down_out
        right_in :: Tight -- -> right_out
    } deriving Show

data TileOutputs = TileOutputs {
        up_out :: Wide,
        left_out :: Wide,
        down_out :: Tight,
        right_out :: Tight
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
    Signal dom TileInputs -> Signal dom TileOutputs -- ooit nog programmen?
tile inp = TileOutputs <$> up_out <*> left_out <*> down_out <*> right_out
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

        up1 = lut 0b0000    (up_in0             `sconcat` up_in1)
        up2 = lut 0b0000    ((up1 +|+ left1)    `sconcat` up_in2)
        left1 = lut 0b0000  (left_in0           `sconcat` left_in1)
        left2 = lut 0b0000  ((left1 +|+ up1)    `sconcat` left_in2) 
        down1 = lut 0b0000  (down_in0           `sconcat` down_in1)
        down2 = lut 0b0000  ((down1 +|+ left2)  `sconcat` down_in0)
        right1 = lut 0b0000 (right_in0          `sconcat` right_in1)
        right2 = lut 0b0000 ((right1 +|+ up2)   `sconcat` right_in0)

        (+|+) = liftA2 or

