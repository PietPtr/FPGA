module Flashing where

import Clash.Prelude hiding (map, repeat, (++), zip, zipWith, take, (:))
import qualified Clash.Prelude as Clash

import Lut2
import ConfigJSON
import Bitstream
import ManualDesign

import Data.List

-- Partially a testbench, partially kind of hardware?

simmable inp = topEntity clockGen resetGen enableGen flashStream inputs
    where
        (flashStream, inputs) = unbundle inp

flash :: Bitstream -> [FPGAOutput 4]
flash stream = take 320 $ simulate simmable $ zip mStream inputs
    where
        inputs = repeat zero
        mStream = Nothing : map Just stream ++ repeat Nothing

        zero = FPGAInput zeroVec zeroVec zeroVec zeroVec
        zeroVec = Clash.replicate d4 0

test :: FPGAConfig -> [FPGAOutput 4]
test config = flash $ bitstream def config