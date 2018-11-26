module Main where

import qualified Lib
import Graphics.Image

main :: IO ()
main = do
    image <- readImageRGB RPU "sierra.jpg"
    Lib.getRandomPixel image >>= print
    Lib.getRandomPixel image >>= print
    Lib.getRandomPixel image >>= print
    displayImage image