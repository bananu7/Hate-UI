module Hate.UI.Util where

import Hate.UI.Types
import Hate.Math

import Hate.Fonts

uiPrint ui str = hatePrint (uiFont . base $ ui) str

between :: (Vec2, Vec2) -> Vec2 -> Bool
between (Vec2 minx miny, Vec2 maxx maxy) (Vec2 x y) = foldl1 (&&) [x >= minx, x <= maxx, y >= miny, y <= maxy]
