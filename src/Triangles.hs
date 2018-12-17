{-# LANGUAGE TupleSections #-}

-- module Triangles
-- ( getRandomPixel
-- , getRandomTriangle
-- , getPointsInTriangle
-- , getTriangleAverageRGB
-- ) where

module Triangles where

import Graphics.Image hiding (map)
import System.Random
import qualified Data.Colour.SRGB as C
import qualified Data.Colour as C
import qualified Graphics.Image.ColorSpace as G

type Image_ = Image VU RGB Double
type Pixel_ = Pixel RGB Double
type Point = (Int, Int)
type Triangle = (Point, Point, Point)

getRandomPixel :: StdGen -> Image_ -> (Int, Int)
getRandomPixel gen image =
    ( getCoord gen  . rows $ image
    , getCoord gen' . cols $ image)
    where
        getCoord :: StdGen -> Int -> Int
        getCoord gen = fst . (flip randomR) gen . (1,)

        gen' = snd . next $ gen

first3 (a : b : c : _) = (a, b, c)

getRandomTriangle :: StdGen -> Image_ -> Triangle
getRandomTriangle gen image =
    first3
    . map (\x -> getRandomPixel x image)
    . iterate (snd . next) $ gen
    
getPointsInTriangle :: Image_ -> Triangle -> [Point]
getPointsInTriangle image triangle
    = filter (isPointInTriangle triangle)
    $ (,) <$> [0..(rows image)] <*> [0..(cols image)]



tosRGB' (G.PixelRGB r g b) = C.sRGB r g b

blendEqually colors = C.affineCombo (zip (repeat fraction) colors) $ head colors
    where
        fraction = 1.0 / (fromIntegral . length $ colors)

getTriangleAverageRGB :: Image_ -> Triangle -> C.Colour Double
getTriangleAverageRGB image triangle = blendEqually . map tosRGB' $  pixels
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
