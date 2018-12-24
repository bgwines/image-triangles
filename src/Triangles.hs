{-# LANGUAGE TupleSections #-}

-- module Triangles
-- ( getRandomPixel
-- , getRandomTriangle
-- , getPointsInTriangle
-- , getTriangleAverageRGB
-- ) where

module Triangles where

import Graphics.Image hiding (map, zipWith, sum, minimum, maximum)
import System.Random
import qualified Data.Colour.SRGB as C
import qualified Data.Colour as C
import qualified Graphics.Image.ColorSpace as G
import Data.List
import Data.Maybe
import Debug.Trace

type Image_ = Image VU RGB Double
type Pixel_ = Pixel RGB Double
type Point = (Int, Int)
type Triangle = (Point, Point, Point)

sharesCoords ((x1, y1), (x2, y2), (x3, y3)) = ((/= 3) . length . nub $ [x1, x2, x3])
                                          || ((/= 3) . length . nub $ [y1, y2, y3])

shoelace :: [Point] -> Double 
shoelace pts = halve . sum $ zipWith (*) (zipWith (+) xs' xs) (zipWith (-) ys' ys)
    where
        showme =  zipWith (*) (zipWith (+) xs' xs) (zipWith (-) ys' ys)
        xs = map snd pts
        ys = map fst pts
        ys' = tail . cycle $ xs
        xs' = tail . cycle $ ys
        halve b =  (fromIntegral b) / 2.0

shoelace' :: [Point] -> Double 
shoelace' [(y1, x1), (y2, x2), (y3, x3)] =  abs $ (* 0.5) . fromIntegral $ x1*y2 + x2*y3 + x3*y1 - x2*y1 - x3*y2 - x1*y3

area :: Triangle -> Double
area (p1, p2, p3) = shoelace' $ [p1, p2, p3]
-- area (p1, p2, p3) = abs . ccArea . swapForCounterClockwise . sortOn fst $ [p1, p2, p3]
    where
        swapForCounterClockwise [a, b, c] = if snd a < snd b
                                               then [a, b, c]
                                               else [b, a, c]
        ccArea [(x1, y1), (x2, y2), (x3, y3)] = 
            (fromIntegral (x1 * y2 + x2 * y3 + x3 * y1
                         - x1 * y3 - x2 * y1 - x3 - y2)) / 2.0

getRandomPixel :: StdGen -> Image_ -> (Int, Int)
getRandomPixel gen image =
    ( getCoord gen  . pred . rows $ image
    , getCoord gen' . pred . cols $ image)
    where
        getCoord :: StdGen -> Int -> Int
        getCoord gen = fst . (flip randomR) gen . (1,)

        gen' = snd . next $ gen

first3 (a : b : c : _) = (a, b, c)

getRandomTriangle :: Image_ -> StdGen -> Triangle
getRandomTriangle image gen = (p1, p2, p3)
    where
        p1 : p2 : _ = map (\x -> getRandomPixel x image) genList
        
        genList = tail . iterate (snd . next) $ gen

        p3 = getThirdPoint p1 p2 gen (pi / 10.0)

-- y = mx + b
-- y = m'x + b'

-- m'x + b' = mx + b
-- m'x - mx = b - b'
-- x(m' - m) = b - b'
-- x = (b - b') / (m' - m)

angleIntersect :: (Point, Double) -> (Point, Double) -> Point
angleIntersect ((y1, x1), angle1) ((y2, x2), angle2) = (round y3, round x3)
    where
        m1 :: Double
        m1 = tan angle1
        m2 :: Double
        m2 = tan angle2

        x3 = (b1 - b2) / (m2 - m1)
        y3 = (m1 * x3) + b1
        y3' = (m2 * x3) + b2
        
        b1 :: Double
        b1 = (fromIntegral y1) - (m1 * (fromIntegral x1))
        b2 :: Double
        b2 = (fromIntegral y2) - (m2 * (fromIntegral x2))

getThirdPoint :: Point -> Point -> StdGen -> Double -> Point
getThirdPoint p1 p2 gen tolerance = angleIntersect (p1, p1From2 + p1Angle) (p2, p2From1 - p2Angle)
    where

        showMe = [p1Angle, p2Angle, p3Angle]

        p2From1 :: Double
        p2From1 = angle p2 p1
        
        p1From2 :: Double
        p1From2 = angle p1 p2

        p3Angle :: Double
        p3Angle = fst $ randomR (thirdpi - tolerance, thirdpi + tolerance) gen

        thirdpi :: Double
        thirdpi = pi / 3.0

        p2Angle :: Double
        p2Angle = fst $ randomR (thirdpi - tolerance, p2Max) (snd . next $ gen)

        p2Max :: Double
        p2Max = {-min (pi - p3Angle - (thirdpi - tolerance))-} (thirdpi + tolerance)

        p1Angle :: Double
        p1Angle = pi - p3Angle - p2Angle

angle :: Point -> Point -> Double
angle (y, x) (fromy, fromx) = atan2 y' x'
    where
        y' = fromIntegral $ y - fromy
        x' = fromIntegral $ x - fromx
    
getPointsInTriangle :: Image_ -> Triangle -> [Point]
getPointsInTriangle image (p1', p2', p3') = (ptsBtween (makeLine p1 p3) (makeLine p1 p2)) ++
                                            (ptsBtween (makeLine p1 p3) (makeLine p2 p3))
    where
        sortedPoints = sortOn snd [p1', p2', p3']

        p1 = sortedPoints !! 0
        p2 = sortedPoints !! 1
        p3 = sortedPoints !! 2

-- getPointsInTriangle image triangle =
    -- = filter (isPointInTriangle triangle)
    -- $ (,) <$> [0..(rows image)] <*> [0..(cols image)]
    



tosRGB' (G.PixelRGB r g b) = C.sRGB r g b

blendEqually colors = C.affineCombo (zip (repeat fraction) colors) $ head colors
    where
        fraction = 1.0 / (fromIntegral . length $ colors)

getTriangleAverageRGB :: Image_ -> Triangle -> C.Colour Double
getTriangleAverageRGB image triangle = traceShow triangle $ blendEqually . map tosRGB' $  pixels
    where
        nPixels :: Pixel RGB Double
        nPixels = fromIntegral $ length pixels

        pixels :: [Pixel_]
        pixels = catMaybes . map (maybeIndex image) $ points

        points :: [Point]
        points = getPointsInTriangle image triangle

-- pointsInTriangle image (p1, p2, p3) = 
--     where
--         sortedPts = sortOn snd [p1, p2, p3]

ptsBtween :: Line -> Line -> [Point]
ptsBtween l1 l2 = concatMap rasterLine [startingX .. endingX]
    where
        startingX = max (startX l1) (startX l2)
        endingX   = min (endX l1) (endX l2)

        rasterLine x = map (\y -> (y, x)) $ range' (yAt l1 x) (yAt l2 x)

        
range' :: Int -> Int -> [Int]
range' a b = [(min a b) .. (max a b)]

yAt :: Line -> Int -> Int
yAt (Line {m = m, b = b}) x = round $ (m * (fromIntegral x)) + b

-- y = mx + b
-- y - mx = b
makeLine :: Point -> Point -> Line
makeLine (y1, x1) (y2, x2) = Line {
    m = slope, 
    b = (fromIntegral y1) - (slope * (fromIntegral x1)),
    startX = min x1 x2,
    endX = max x1 x2
                                  }
    where
        slope = (y1 - y2) `doubleDiv` (x1 - x2)

doubleDiv a b = (fromIntegral a) / (fromIntegral b)

data Line = Line
    {
        m :: Double,
        b :: Double,
        startX :: Int,
        endX :: Int
    }

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
