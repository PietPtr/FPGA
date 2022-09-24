module TB where

import Clash.Prelude

import Lut2

import qualified Data.List as L


simmable inp = manualFPGA flash fpgaIn
    where
        (flash, fpgaIn) = unbundle inp

loadOnes = simulate @System simmable ones
    where
        ones = L.zip (L.take 160 $ L.repeat $ Just 1) (L.repeat def)


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