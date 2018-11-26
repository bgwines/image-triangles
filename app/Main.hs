module Main where

import qualified Lib
import Graphics.Image

ii :: IO (Image VU RGB Double)
ii = readImageRGB VU "sierra.jpg"

main :: IO ()
main = do
    image <- readImageRGB VU "sierra.jpg"
    displayImage image