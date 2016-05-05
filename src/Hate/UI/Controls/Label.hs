{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hate.UI.Controls.Label where

import Hate.UI.Types
import Hate.UI.Util

import Hate.Graphics
import Hate.Math

import Hate.Fonts
import Hate.Fonts.Loader

data Label s = Label Vec2 (Binding s String)

instance Element s (Label s) where
    drawElement ub s (Label p (PlainValue str)) = (translate p) <$> uiPrint ub str
    drawElement ub s (Label p (Binding b)) = (translate p) <$> uiPrint ub (b s)

label :: Vec2 -> (Binding s String) -> AnyElement s
label p b = AnyElement $ Label p b
