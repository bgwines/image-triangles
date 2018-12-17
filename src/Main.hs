module Main where

import qualified Triangles as Tri
import qualified Render    as Ren
import Graphics.Image hiding (map)
import System.Random
import qualified Graphics.Image.ColorSpace as G
import qualified Data.Colour.SRGB as C
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude



genList = map mkStdGen . randoms

singleTriangle image gen = reflectY . Ren.makeTriangle (Ren.toPointList dims t) $ col
    where
        t = Tri.getRandomTriangle gen image
        col = Tri.getTriangleAverageRGB image $ t
        dims = (cols image, rows image)

main :: IO ()
main = do
    image <- readImageRGB VU "sierra.jpg"
    gen <- getStdGen
    print gen
    let t = Tri.getRandomTriangle gen image
    let col = Tri.getTriangleAverageRGB image t
    print col
    let dims = (cols image, rows image)
    mainWith . mconcat . take 50 . map (singleTriangle image) . genList $ gen
