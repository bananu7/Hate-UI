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
data Button s = Button Vec2 (Label s) (s -> s)

-- |Yes this is hardcoded and it's terrible but it's just for now
buttonSize = (Vec2 50 20)

instance Element s (Button s) where
    drawElement s (Button p lab _) = (translate p) <$> drawElement s lab ++ [line (Vec2 0 0) buttonSize]
    click mp (Button pos _ action) = if between (pos, pos + buttonSize) mp 
        then Just . state $ ((),) . action
        else Nothing

button :: forall s. Vec2 -> String -> (s -> s) -> AnyElement s
button pos str action = AnyElement $ (Button pos (Label (Vec2 1 1) (PlainValue str) :: Label s) action :: Button s)
