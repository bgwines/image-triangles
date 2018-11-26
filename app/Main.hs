module Main where

import qualified Lib
import Graphics.Image

i :: IO (Image VU RGB Double)
i = readImageRGB VU "sierra.jpg"

main :: IO ()
main = do
    image <- readImageRGB VU "sierra.jpg"
    Lib.getRandomPixel image >>= print
    Lib.getRandomPixel image >>= print
    Lib.getRandomPixel image >>= print
    displayImage image