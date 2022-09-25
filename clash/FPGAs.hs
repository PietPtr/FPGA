module FPGAs where

import Clash.Prelude hiding (Bit, or)

import Lut2


fpga1x1 :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Bit) -> Signal dom (FPGAInput 1) -> Signal dom (FPGAOutput 1)
fpga1x1 flash inp = out <$> rows
    where
        (r1, r1conf) = row1 flash  $ mkRowInput <$> inp <*> 0 <*> (downf_in <$> inp) <*> (downf_in <$> inp)
        rows = bundle (r1:>Nil)

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


row1 flash row = (out <$> tiles, t1conf)
    where
        (t1, t1conf) = tile flash (mkTileInput <$> row <*> 0 <*> (rightr_in <$> row) <*> (leftr_in <$> row))
        tiles = bundle (t1:>Nil)

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



fpga2x2 :: HiddenClockResetEnable dom =>
    Signal dom (Maybe Bit) -> Signal dom (FPGAInput 2) -> Signal dom (FPGAOutput 2)
fpga2x2 flash inp = out <$> rows
    where
        (r1, r1conf) = row2 flash  $ mkRowInput <$> inp <*> 0 <*> (downr_out <$> r2) <*> (downf_in <$> inp)
        (r2, r2conf) = row2 r1conf $ mkRowInput <$> inp <*> 1 <*> (downf_in <$> inp) <*> (upr_out <$> r1)
        rows = bundle (r1:>r2:>Nil)

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

row2 flash row = (out <$> tiles, t2conf)
    where
        (t1, t1conf) = tile flash  (mkTileInput <$> row <*> 0 <*> (left_out <$> t2) <*> (leftr_in <$> row))
        (t2, t2conf) = tile t1conf (mkTileInput <$> row <*> 1 <*> (rightr_in <$> row) <*> (right_out <$> t1))
        tiles = bundle (t1:>t2:>Nil)

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