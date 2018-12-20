module Render where

import Diagrams.TrailLike


import qualified Triangles as Tri
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


makeTriangle :: [Point V2 Double] -> Colour Double -> Diagram SVG
makeTriangle verts col = fromVertices verts
                       # mapLoc closeLine
                       # strokeLocLoop 
                       # fc col
                       # lw 0
                       # opacity 0.2


-- tupleFromIntegral :: (Int, Int) -> (Int, Int) -> (Double, Double)
tupleFromIntegral (cols, rows) (a, b) = (fromIntegral b, fromIntegral a)
-- tupleFromIntegral (cols, rows) (a, b) = ((a `divv` cols) , ((rows - b) `divv` rows))

divv :: Int -> Int -> Double
a `divv` b = (fromIntegral a) / (fromIntegral b)

toPointList :: (Int, Int) -> Tri.Triangle -> [Point V2 Double]
toPointList dims (a, b, c) = map (p2 . tupleFromIntegral dims) [a, b, c]

-- renderTriangle = makeTriangle (map p2 [(0.0,0.0), (0.1,0.1), (0.2,0.2)]) blue
