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
    let triangleList = sortOn (Tri.area) . withStrategy (parListChunk 50 rseq) . take 600 . map (Tri.getRandomTriangle image) . genList $ gen
    mainWith . mconcat . map (renderTri image) $ triangleList
