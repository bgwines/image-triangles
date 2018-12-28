{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Triangles as Tri
import qualified Render    as Ren
import Graphics.Image as Img hiding (map, zipWith)
import System.Random
import qualified Graphics.Image.ColorSpace as G
import qualified Data.Colour.SRGB as C
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Data.List
import Control.Parallel.Strategies
import qualified Graphics.Image.Interface.Repa as Img
import qualified Data.Array.Repa as R
import qualified Data.Vector as Vec
import qualified Debug.Trace as DT

data Options = Options {
                    nRounds :: Int,
                    nTrianglesPerRound :: Int,
                    gen :: Maybe StdGen
               }

defaultOpts  = Options {
                    nRounds = 3,
                    nTrianglesPerRound = 100,
                    gen = Nothing
                  }



genList :: StdGen -> [StdGen]
genList = map mkStdGen . randoms

tosRGB' :: (Ord b, Floating b) => Pixel G.RGB b -> Colour b
tosRGB' (G.PixelRGB r g b) = C.sRGB r g b

convImage :: Image VU G.RGB Double -> Vec.Vector (Colour Double)
convImage = Vec.map tosRGB' . Vec.convert . R.toUnboxed . Img.toRepaArray

renderTriangles
    :: Vec.Vector (Colour Double)
    -> Int
    -> (Int, Int)
    -> Double
    -> Int
    -> StdGen
    -> Int
    -> QDiagram SVG V2 Double Any
renderTriangles image nRounds dimensions areaCoeff nTrianglesPerRound gen round'
    = mconcat . map renderTriangle $ triangles
    where
        renderTriangle t = reflectY $ Ren.makeTriangle (Ren.toPointList dimensions t) col opacity
            where
                col :: C.Colour Double
                col = Tri.getTriangleAverageRGB image t dimensions

        numCandidates = round $ (fromIntegral nTrianglesPerRound) / areaCoeff

        opacity :: Double
        opacity = fromIntegral round' / fromIntegral nRounds


        triangles
            = take nTrianglesPerRound
            $ sortOn Tri.area
            . take numCandidates
            . filter (not . Tri.sharesCoords)
            . map (Tri.getRandomTriangle dimensions)
            . genList
            $ gen

genImage :: String -> Int -> Int -> Double ->  IO (Diagram B)
genImage name areaCoeff = do
    let 
    image <- Img.readImageRGB VU name
    let img' = convImage image
    let dimensions = (rows image, cols image)
    let renderTriangles' = renderTriangles img' nRounds dimensions areaCoeff nTrianglesPerRound
    gen <- if randSeed == 0 then getStdGen else return $ mkStdGen randSeed
    let triangles = zipWith renderTriangles' (genList gen) $ [1..nRounds]
    return $ center . mconcat . withStrategy (parListChunk 800 rseq) $ reverse triangles

main :: IO ()
main = mainWith genImage
