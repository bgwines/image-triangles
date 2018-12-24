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


genList = map mkStdGen . randoms


renderTri image t = reflectY . Ren.makeTriangle (Ren.toPointList dims t) $ col
    where
        dims = (cols image, rows image)
        col = Tri.getTriangleAverageRGB image $ t


main = mainWith genImage


genImage :: String -> Int -> Double -> Int -> IO (Diagram B)
genImage name triNum areaCoeff randSeed = do
    image <- readImageRGB VU name
    gen <- if randSeed == 0 then getStdGen else return $ mkStdGen randSeed
    print gen
    let dims = (cols image, rows image)
    let numCandidates = round $ (fromIntegral triNum) / areaCoeff
    let triangleList = take triNum $ sortOn Tri.area . take numCandidates . filter (not . Tri.sharesCoords) . map (Tri.getRandomTriangle image) . genList $ gen
    -- print $ map Tri.area $ take 20 $ triangleList
    -- print $ Tri.area . last $ triangleList
    return $ center . mconcat . withStrategy (parListChunk 50 rseq) . map (renderTri image) $ triangleList
