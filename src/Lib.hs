module Lib
( displayGradGrayImage
) where


import Graphics.Image
-- import Graphics.Image.Interface.Elevator


makeGradGrayImage :: Image RPU Y Double
makeGradGrayImage = makeImageR RPU (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)

displayGradGrayImage :: IO ()
displayGradGrayImage = displayImage makeGradGrayImage