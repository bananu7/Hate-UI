{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hate.UI.Controls.Window
    ( window
    )
where

import Hate.UI.Types
import Hate.UI.Controls.Label
import Hate.UI.Util

import Hate.Graphics
import Hate.Math

import Control.Monad.State (state)

-- In order to keep things simple, button cannot nest arbitrary controls
data Window s = Window Vec2 Vec2 [AnyElement s]

instance Element s (Window s) where
    drawElement ub s (Window pos sz children) = translate pos <$> (box (Vec2 0 0) sz) ++ concatMap (drawElement ub s) children

    {-
    click mp (Button pos _ action) = if between (pos, pos + buttonSize) mp 
        then Just . state $ ((),) . action
        else Nothing
    -}

window :: forall s. Vec2 -> Vec2 -> [AnyElement s] -> AnyElement s
window pos sz children = AnyElement $ Window pos sz children
