{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI.Controls.Button where

import Hate.UI.Types
import Hate.UI.Controls.Label
import Hate.UI.Util

import Hate.Graphics
import Hate.Math

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button Vec2 (Label s)

-- |Yes this is hardcoded and it's terrible but it's just for now
buttonSize = (Vec2 10 10)

instance Element s (Button s) where
    drawElement s (Button p lab) = (translate p) <$> drawElement s lab ++ [line (Vec2 0 0) buttonSize]
    click mp (Button pos _)= if between (pos, pos + buttonSize) mp then Just $ do
        error "clicked!"
        else
            Nothing

button :: forall s. Vec2 -> String -> AnyElement s
button pos str = AnyElement $ (Button pos (Label (Vec2 1 1) (PlainValue str) :: Label s) :: Button s)
