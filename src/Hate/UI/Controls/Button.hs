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
    drawElement s (Button p sz lab _) = (translate p) <$> drawElement s lab ++ (box (Vec2 0 0) sz)
    click mp (Button pos sz _ action) = if between (pos, pos + sz) mp 
        then Just . state $ ((),) . action
        else Nothing

button :: forall s. Vec2 -> Vec2 -> String -> (s -> s) -> AnyElement s
button pos sz str action = AnyElement $ (Button pos sz (Label (Vec2 1 1) (PlainValue str) :: Label s) action :: Button s)
