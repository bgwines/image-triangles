module Main where

import qualified Triangles as Tri
import qualified Render    as Ren
import Graphics.Image as Img hiding (map, zipWith)
import System.Random
import qualified Graphics.Image.ColorSpace as G
import qualified Data.Colour.SRGB.Linear as CL
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Debug.Trace
import Data.List
import Control.Parallel.Strategies
import qualified Graphics.Image.Interface.Repa as Img
import qualified Data.Array.Repa as R
import qualified Data.Vector as Vec
import qualified Debug.Trace as DT

data Options = Options {
                    numTriangles :: Int,
                    gen :: Maybe StdGen
               }

-- modify this to your liking
defaultOpts  = Options {
                    numTriangles = 10000,
                    gen = Nothing
                  }

genList :: StdGen -> [StdGen]
genList = map mkStdGen . randoms

-- CL.rgb might be the wrong fn...
tosRGB' :: (Ord b, Floating b) => Pixel G.RGB b -> CL.Colour b
tosRGB' (G.PixelRGB r g b) = CL.rgb r g b


convImage = Vec.map tosRGB' . Vec.convert . R.toUnboxed . Img.toRepaArray

-- progress goes from 0 to 1 the farther we get along the process
-- note, 0 represents the topmost triangle
renderTri :: Vec.Vector (Colour Double) -> (Int, Int) -> StdGen -> Double -> QDiagram SVG V2 Double Any
renderTri image dimensions gen progress = Ren.makeTriangle (Ren.toPointList dimensions triangle) color opacity'
    where

        triangle = Tri.getRandomTriangle image dimensions (Just area) gen
        
        color = Tri.getTriangleAverageRGB image triangle dimensions
        
        -- the following should be considered triangle shaders
        -- modify them to your liking, their outputs are expected to be in [0, 1]
        
        opacity' = 0.4 + (1 - progress) * 0.6
        
        area = (progress ** 2) * 0.2




genImage :: String -> IO (Diagram B)
genImage name = do
    let (Options {numTriangles = numTriangles, gen = gen'}) = defaultOpts
    gen'' <- case gen' of
                        Nothing -> getStdGen
                        Just a  -> return a
    image <- Img.readImageRGB VU name
    let img' = convImage image
    let dimensions = (rows image, cols image)
    print gen''
    let progressList = map (/ (fromIntegral numTriangles))  [0.0 .. (fromIntegral numTriangles)]
    return $ center . reflectY . mconcat . withStrategy (parListChunk 75 rseq) $ zipWith (renderTri img' dimensions) (genList gen'') progressList

main :: IO ()
main = mainWith genImage
