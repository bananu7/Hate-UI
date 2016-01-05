{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI.Controls.Button where

import Hate.UI.Types
import Hate.UI.Controls.Label

import Hate.Graphics
import Hate.Math

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button Vec2 (Label s)

instance Element s (Button s) where
    drawElement s (Button p lab) = (translate p) <$> drawElement s lab ++ [line (Vec2 0 0) (Vec2 10 10)]

button :: forall s. Vec2 -> String -> AnyElement s
button pos str = AnyElement $ (Button pos (Label (Vec2 1 1) (PlainValue str) :: Label s) :: Button s)
