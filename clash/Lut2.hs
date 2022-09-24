module Top where

import Clash.Prelude hiding (Bit, or)

type Bit = BitVector 1

type LutSelect = BitVector 2
type LutOut = BitVector 1
type Table = BitVector 4

access a b = fromIntegral $ a ! b

routeFlash :: Maybe b -> b -> Maybe b
routeFlash flash val = (const val) <$> flash

lut :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Bit) -> Signal dom Bit -> Signal dom Bit ->
    (Signal dom Bit, Signal dom Bit, Signal dom (Maybe Bit))
lut flash sel1 sel2 = (
    access <$> out <*> 0, 
    access <$> out <*> 1, 
    routeFlash <$> flash <*> regmuxconf)
    where
        selector = (++#) <$> sel1 <*> sel2
        out = mux ((== 1) <$> regmuxconf) lutOut regOut

        lutOut = select <$> configState <*> selector
        select state selector = (state ! (selector `shiftL` 1))
                            `cons` (state ! (selector `shiftL` 1 + 1))

        regOut = register (0 :: BitVector 2) lutOut

        configState = configReg (0 :: BitVector 9) flash
        regmuxconf = access <$> configState <*> 8

        cons a b = (fromIntegral a :: Bit) ++# (fromIntegral b :: Bit)

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
    } deriving Show

data TileInputRow (n :: Nat) = TileInputRow {
        upr_in :: Vec n Bit,
        leftr_in :: Bit,
        downr_in :: Vec n Bit,
        rightr_in :: Bit
    }

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
    }

data TileOutputRow (n :: Nat) = TileOutputRow {
        upr_out :: Vec n Bit,
        leftr_out :: Bit,
        downr_out :: Vec n Bit,
        rightr_out :: Bit
    }


-- row :: (HiddenClockResetEnable dom, KnownNat r) =>
--     Signal dom Bool -> Signal dom Bit -> 
--     Signal dom (TileInputRow r) -> 
--     Signal dom (TileOutputRow r)
-- row flashing config row = undefined
--     where
--         tiles = scanl holeja (tile flashing config) (upr_in <$> row)

manualRow flash row = (t1, t2conf)
    where
        (t1, t1conf) = tile flash (mkTileInput <$> row <*> 0 <*> (leftr_in <$> row) <*> (right_out <$> t2))
        (t2, t2conf) = tile t1conf (mkTileInput <$> row <*> 1 <*> (left_out <$> t1) <*> (rightr_in <$> row))


        mkTileInput row idx left right = TileInputs {
                up_in = (upr_in row) !! idx,
                left_in = left,
                down_in = (downr_in row) !! idx,
                right_in = right
            }

or :: Bit -> Bit -> Bit
or 0 0 = 0
or _ _ = 1

tile :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Bit) -> Signal dom TileInputs -> 
    (Signal dom TileOutputs, Signal dom (Maybe Bit))
tile flash inp = (TileOutputs <$> up_out <*> left_out <*> down_out <*> right_out, horzconf)
    where
        up_out = up
        left_out = left
        down_out = down +|+ l_in
        right_out = right +|+ d_in

        u_in = up_in <$> inp
        l_in = left_in <$> inp
        d_in = down_in <$> inp
        r_in = right_in <$> inp

        (up, down, vertconf)    = lut flash u_in (d_in +|+ right)
        (left, right, horzconf) = lut flash r_in (l_in +|+ down)  

        (+|+) = liftA2 or
