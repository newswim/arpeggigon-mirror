module RMCA.GUI.Shapes where

import Graphics.Rendering.Cairo

-- Draws a regular hexagon
--
-- Colors are given in RGB format, coefficient goes from 0 to 1
hexagon :: (Double, Double, Double) -- Background color
        -> (Double, Double, Double) -- Frame color
        -> (Double, Double) -- Center
        -> Double -- Width
        -> Render ()
hexagon (backR, backG, backB) (frameR, frameG, frameB) (x,y) w = do
  setSourceRGB frameR frameG frameB
  setLineWidth (0.01 * w)

  let a = 0.5*w
      b = 0.87*w

  moveTo (x+a) (y-b)
  lineTo (x-a) (y-b)
  lineTo (x-w) y
  lineTo (x-a) (y+b)
  lineTo (x+a) (y+b)
  lineTo (x+w) y
  closePath
  strokePreserve
  setSourceRGB backR backG backB
  fill
  return ()

pnw = 200
pnh = 200

main = withImageSurface FormatARGB32 pnw pnh
  (\srf -> do renderWith srf (hexagon (1,1,1) (0,0,0)
                              (fromIntegral pnw/2,fromIntegral pnh/ 2) (fromIntegral (pnw `quot` 2)))
              surfaceWriteToPNG srf "myDraw.png")
