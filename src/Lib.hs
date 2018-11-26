{-# LANGUAGE TupleSections #-}

module Lib
( getRandomTriangle
, getPointsInTriangle
, getTriangleAverageRGB
) where


import Graphics.Image hiding (map)
import qualified System.Random

type Image_ = Image VU RGB Double
type Pixel_ = Pixel RGB Double
type Point = (Int, Int)
type Triangle = (Point, Point, Point)

getRandomPixel :: Image_ -> IO (Int, Int)
getRandomPixel image = do
    -- TODO: something fancy with bifunctors
    x <- getCoord . rows $ image
    y <- getCoord . cols $ image
    return (x, y)
    where
        getCoord :: Int -> IO Int
        getCoord = System.Random.getStdRandom . System.Random.randomR . (1,)

getRandomTriangle :: Image_ -> IO Triangle
getRandomTriangle image = do
    a <- Lib.getRandomPixel image
    b <- Lib.getRandomPixel image
    c <- Lib.getRandomPixel image
    return (a, b, c)
    
getPointsInTriangle :: Image_ -> Triangle -> [Point]
getPointsInTriangle image triangle
    = filter (isPointInTriangle triangle)
    $ (,) <$> [0..(rows image)] <*> [0..(cols image)]

getTriangleAverageRGB :: Image_ -> Triangle -> Pixel_
getTriangleAverageRGB image triangle = (foldl1 (+) pixels) / nPixels
    where
        nPixels :: Pixel RGB Double
        nPixels = fromIntegral $ length pixels

        pixels :: [Pixel_]
        pixels = map (index image) points

        points :: [Point]
        points = getPointsInTriangle image triangle

isPointInTriangle :: Triangle -> Point -> Bool
isPointInTriangle (v1, v2, v3) pt = not (has_neg && has_pos)
    where
        d1 = sign pt v1 v2
        d2 = sign pt v2 v3
        d3 = sign pt v3 v1

        has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0)
        has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0)

        sign :: Point -> Point -> Point -> Int
        sign p1 p2 p3 = (fst p1 - fst p3) * (snd p2 - snd p3) - (fst p2 - fst p3) * (snd p1 - snd p3)
