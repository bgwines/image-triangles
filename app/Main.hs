module Main where

import Lib
import Graphics.Image

main :: IO ()
main = do
    image <- readImageRGB VU "sierra.jpg"
    displayImage image