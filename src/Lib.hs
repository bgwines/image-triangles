{-# LANGUAGE TupleSections #-}

module Lib
( getRandomPixel
) where


import Graphics.Image
import qualified System.Random

type Image_ = Image RPU RGB Double

getRandomPixel :: Image_ -> IO (Int, Int)
getRandomPixel image = do
    -- TODO: something fancy with bifunctors
    x <- getCoord . rows $ image
    y <- getCoord . cols $ image
    return (x, y)
    where
        getCoord :: Int -> IO Int
        getCoord = System.Random.getStdRandom . System.Random.randomR . (1,)

-- makeGradGrayImage :: Image_
-- makeGradGrayImage = makeImageR RPU (200, 200) (\(i, j) -> PixelRGB $ fromIntegral (i*j)) / (200*200)

-- displayGradGrayImage :: IO ()
-- displayGradGrayImage = displayImage makeGradGrayImage