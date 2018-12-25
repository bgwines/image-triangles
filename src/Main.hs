module Main where

import qualified Triangles as Tri
import qualified Render    as Ren
import Graphics.Image hiding (map)
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

genList = map mkStdGen . randoms

tosRGB' (G.PixelRGB r g b) = C.sRGB r g b

convImage = Vec.map tosRGB' . Vec.convert . R.toUnboxed . Img.toRepaArray

renderTri dims image t = reflectY . Ren.makeTriangle (Ren.toPointList dims t) $ col
    where
        col = Tri.getTriangleAverageRGB image t dims

main = mainWith genImage


genImage :: String -> Int -> Double -> Int -> IO (Diagram B)
genImage name triNum areaCoeff randSeed = do
    image <- readImageRGB VU name
    gen <- if randSeed == 0 then getStdGen else return $ mkStdGen randSeed
    print gen
    let dims = (rows image, cols image)
    let image' = convImage image
    let numCandidates = round $ (fromIntegral triNum) / areaCoeff
    let triangleList = take triNum $ sortOn Tri.area . take numCandidates . filter (not . Tri.sharesCoords) . map (Tri.getRandomTriangle dims) . genList $ gen
    -- print $ map Tri.area $ take 20 $ triangleList
    -- print $ Tri.area . last $ triangleList
    let img' = convImage image
    return $ center . mconcat . withStrategy (parListChunk 500 rseq) . map (renderTri dims img') $ triangleList
