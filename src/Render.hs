module Render where

import Diagrams.TrailLike



import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine



makeTriangle :: [Point V2 Double] -> Colour Double -> Diagram SVG
makeTriangle verts col = fromVertices verts # fc col


renderTriangle = makeTriangle (map p2 [(0.0,0.0), (0.1,0.1), (0.2,0.2)]) blue
