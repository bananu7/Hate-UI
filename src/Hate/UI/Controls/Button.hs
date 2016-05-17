{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hate.UI.Controls.Button where

import Hate.UI.Types
import Hate.UI.Controls.Label
import Hate.UI.Util

import Hate.Graphics
import Hate.Math
import Debug.Trace

import Control.Monad.State (state)

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button !Vec2 !Vec2 !Bool (Label s) (s -> s)

instance Element s (Button s) where
    drawElement ub s (Button p sz hover lab _) = (translate p) <$> buttonBox ++ drawElement ub s lab
        where
            buttonBox = if hover then (filledBox sz)
                                 else (box (Vec2 0 0) sz)

    click mp (b@(Button pos sz _ _ action)) = if between (pos, pos + sz) mp 
        then (action, enlargeButton b)
        else (id, b)

    mouseMove mp (b@(Button pos sz _ _ action)) =
        if between (pos, pos + sz) mp 
            then (id, setHover True b)
            else (id, setHover False b)

-- TEMP: this is an example of how an effect such as OnHover could be implemented
enlargeButton (Button p (Vec2 sx sy) h lab act) = Button p (Vec2 (sx + 2) sy) h lab act

setHover :: Bool -> Button s -> Button s
setHover h (Button p s _ l f) = Button p s h l f

button :: forall s. Vec2 -> Vec2 -> String -> (s -> s) -> Button s
button pos sz str action = buttonBnd pos sz (PlainValue str) action

buttonBnd :: forall s. Vec2 -> Vec2 -> Binding s String -> (s -> s) -> Button s
buttonBnd pos sz bnd action = (Button pos sz False (Label (Vec2 1 1) bnd :: Label s) action :: Button s)
