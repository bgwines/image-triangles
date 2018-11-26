{-# LANGUAGE TupleSections #-}

module Lib
( getRandomPixel
, getPointsInTriangle
, getPointsInRandomTriangle
) where


import Graphics.Image
import qualified System.Random

type Image_ = Image RPU RGB Double
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

getPointsInTriangle :: Image_ -> Triangle -> [Point]
getPointsInTriangle image triangle
    = filter (isPointInTriangle triangle)
    $ (,) <$> [0..(rows image)] <*> [0..(cols image)]

getPointsInRandomTriangle :: Image_ -> IO [Point]
getPointsInRandomTriangle image = do
    a <- getRandomPixel image
    b <- getRandomPixel image
    c <- getRandomPixel image
    return $ getPointsInTriangle image (a, b, c)        

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
