{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI where

import Hate.Graphics
import Hate.Math
import qualified Hate.Events.Types as Hate

import Hate.Fonts
import Hate.Fonts.Loader

import Control.Monad.Reader
import Control.Monad.State

-- |This is an equivalent of the state monad, but tailored for the UI.
-- We'll see how useful this approach is.
class HasUI s where
    getUI :: s -> UI s

data UIBase = UIBase {
    uiFont :: Font
}

data UI s = UI {
    base :: UIBase,
    elements :: [AnyElement s]
}

makeUI :: (String, String) -> [AnyElement s]-> IO (UI s)
makeUI (pathFontData, pathFontSprite) elems = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) elems

drawUI :: HasUI s => s -> [DrawRequest]
drawUI s = concatMap (drawElement s) (elements . getUI $ s)

class Element s a where
    drawElement :: HasUI s => s -> a -> [DrawRequest]

data AnyElement s = forall e. Element s e => AnyElement e
instance Element s (AnyElement s) where
    drawElement s (AnyElement e) = drawElement s e

-- |Represents a binding to a type of value a inside of state s
data Binding s a = PlainValue a | Binding (s -> a)

-- Here come the actual controls
data Label s = Label Vec2 (Binding s String)

label :: Vec2 -> (Binding s String) -> AnyElement s
label p b = AnyElement $ Label p b

instance Element s (Label s) where
    drawElement s (Label p (PlainValue str)) = (translate p) <$> uiPrint (getUI s) str
    drawElement s (Label p (Binding b)) = (translate p) <$> uiPrint (getUI s) (b s)

uiPrint ui str = hatePrint (uiFont . base $ ui) str

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button Vec2 (Label s)

button :: forall s. Vec2 -> String -> AnyElement s
button pos str = AnyElement $ (Button pos (Label pos (PlainValue str) :: Label s) :: Button s)

instance Element s (Button s) where
    drawElement s (Button p lab) = (translate p) <$> drawElement s lab ++ [line (Vec2 0 0) (Vec2 10 10)]

--data ImageButton = ImageButton Hate.Sprite

{-
instance EventReceiver Button where
    accept MouseButtonState'Pressed = id
    accept _ = id
-}
