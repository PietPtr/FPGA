module Main where

import Clash.Prelude hiding (Bit)

import qualified Data.Text.IO as TIO

import Lut2
import Flashing

import qualified Data.List as L


main :: IO ()
main = putStrLn $ show $ L.length $ flash [1,1..]