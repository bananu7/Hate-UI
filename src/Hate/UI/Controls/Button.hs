{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hate.UI.Controls.Button (
      button
    , buttonBnd
    , Button()
    ) where

import Hate.UI.Types
import Hate.UI.Controls.Label
import Hate.UI.Util

import Hate.Graphics hiding (size)
import Hate.Math hiding (position)

import Control.Lens
import Control.Monad.State (state)

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button {
    _buttonPosition :: Vec2,
    _buttonSize :: Vec2,
    _buttonHover :: Bool,
    _ButtonLabel :: (Label s),
    _ButtonAction :: (s -> s)
}
makeLenses ''Button
makeFields ''Button

instance Element s (Button s) where
    drawElement ub s btn = (translate (btn ^. position)) <$> buttonBox ++ drawElement ub s (btn ^. buttonLabel)
        where
            buttonBox = if btn ^. hover then filledBox (btn ^. size)
                                        else (box (Vec2 0 0) (btn ^. size))

    handleEvent (UIEvent'MouseDown _ mp) b =
        if insideControl b mp 
            then (b ^. buttonAction, enlargeButton b)
            else (id, b)

    handleEvent (UIEvent'MouseMove mp) b =
        if insideControl b mp 
            then (id, setHover True b)
            else (id, setHover False b)

    handleEvent _ b = (id, b)

-- TEMP: this is an example of how an effect such as OnHover could be implemented
enlargeButton (Button p (Vec2 sx sy) h lab act) = Button p (Vec2 (sx + 2) sy) h lab act

setHover :: Bool -> Button s -> Button s
setHover h (Button p s _ l f) = Button p s h l f

button :: forall s. Vec2 -> Vec2 -> String -> (s -> s) -> Button s
button pos sz str action = buttonBnd pos sz (PlainValue str) action

buttonBnd :: forall s. Vec2 -> Vec2 -> Binding s String -> (s -> s) -> Button s
buttonBnd pos sz bnd action = (Button pos sz False (Label (Vec2 1 1) bnd :: Label s) action :: Button s)
