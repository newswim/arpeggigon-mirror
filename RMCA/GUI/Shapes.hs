module RMCA.GUI.Shapes where

import Graphics.Rendering.Cairo

-- Draws a regular hexagon
--
-- Colors are given in RGB format, coefficient goes from 0 to 1
hexagon :: (Double, Double, Double) -- Background color
        -> (Double, Double, Double) -- Frame color
        -> Double -- Width
        -> Render ()
hexagon (backR, backG, backB) (frameR, frameG, frameB) w = do
  setSourceRGB frameR frameG frameB
  setLineWidth (0.1 * w)
  return ()
