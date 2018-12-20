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

main :: IO ()
main = do
    image <- readImageRGB VU "sierra.jpg"
    gen <- getStdGen
    print gen
    let dims = (cols image, rows image)
    let triangleList = sortOn (negate . Tri.area) . take 150 . map (Tri.getRandomTriangle image) . genList $ gen
    -- print $ map Tri.area $ take 20 $ triangleList
    -- print $ Tri.area . last $ triangleList
    mainWith . mconcat . withStrategy (parListChunk 50 rseq) . map (renderTri image) $ triangleList
