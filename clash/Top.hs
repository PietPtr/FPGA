module Top where

import Clash.Prelude

type LutIn = BitVector 2
type LutOut = Bit
type Table = BitVector 4

-- TODO: programmability?
lut :: HiddenClockResetEnable dom =>
    Table -> Signal dom LutIn -> Signal dom LutOut
lut config selector = (!) <$> state <*> selector
    where
        state' = id state -- TODO: hier programmability somehow?, shift regje toolen?
        state = register config state'