module Hate.UI.Util where

import Hate.UI.Types
import Hate.Math
import Hate.Fonts
import Hate.Graphics

uiPrint :: UIBase -> String -> [DrawRequest]
uiPrint ui str = hatePrint (uiFont $ ui) str

between :: (Vec2, Vec2) -> Vec2 -> Bool
between (Vec2 minx miny, Vec2 maxx maxy) (Vec2 x y) = foldl1 (&&) [x >= minx, x <= maxx, y >= miny, y <= maxy]

box :: Vec2 -> Vec2 -> [DrawRequest]
box (Vec2 ax ay) (Vec2 bx by) = [
    line (Vec2 ax ay) (Vec2 bx ay),
    line (Vec2 bx ay) (Vec2 bx by),
    line (Vec2 bx by) (Vec2 ax by),
    line (Vec2 ax by) (Vec2 ax ay)
    ]


filledBox :: Vec2 -> [DrawRequest]
filledBox (Vec2 w h) = [DrawRequest quad Nothing one FanVertexLayout one (SolidColorPipeline (Vec4 1.0 0.0 0.0 1.0))]
    where
        quad = [Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h]
