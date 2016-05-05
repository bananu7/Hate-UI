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

import Control.Monad.State (state)

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button Vec2 Vec2 (Label s) (s -> s)

instance Element s (Button s) where
    drawElement ub s (Button p sz lab _) = (translate p) <$> drawElement ub s lab ++ (box (Vec2 0 0) sz)
    click mp (b@(Button pos sz _ action)) = if between (pos, pos + sz) mp 
        then (action, enlargeButton b)
        else (id, b)

-- TEMP: this is an example of how an effect such as OnHover could be implemented
enlargeButton (Button p (Vec2 sx sy) lab act) = Button p (Vec2 (sx + 2) sy) lab act

button :: forall s. Vec2 -> Vec2 -> String -> (s -> s) -> Button s
button pos sz str action = buttonBnd pos sz (PlainValue str) action

buttonBnd :: forall s. Vec2 -> Vec2 -> Binding s String -> (s -> s) -> Button s
buttonBnd pos sz bnd action = (Button pos sz (Label (Vec2 1 1) bnd :: Label s) action :: Button s)
