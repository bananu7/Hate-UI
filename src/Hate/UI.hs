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

{-
data UIEvent =
      ClickEvent GLFW.MouseButton
    | MouseOverEvent
    | MouseOffEvent

class EventReceiver r where
    accept :: UIEvent -> r -> r

class (EventReceiver a, Show a) => Element a where

-}

{-
data ElementNode = forall a. Element a => ElementNode {
      value :: a
    , children :: [ElementNode]
}
-}

data UIBase = UIBase {
    uiFont :: Font
}

data UI s = UI {
    base :: UIBase,
    elements :: [AnyElement s]
    --, rootElement :: ElementNode
}

makeUI :: (String, String) -> [AnyElement s]-> IO (UI s)
makeUI (pathFontData, pathFontSprite) elems = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) elems

drawUI :: UI s -> s -> [DrawRequest]
drawUI ui s = concatMap (drawElement ui s) $ elements ui

class Element s a where
    drawElement :: UI s -> s -> a -> [DrawRequest]

data AnyElement s = forall e. Element s e => AnyElement e
instance Element s (AnyElement s) where
    drawElement ui s (AnyElement e) = drawElement ui s e

-- |Represents a binding to a type of value a inside of state s
data Binding s a = PlainValue a | Binding (s -> a)

-- Here come the actual controls
data Label s = Label Vec2 (Binding s String)

label :: Vec2 -> (Binding s String) -> AnyElement s
label p b = AnyElement $ Label p b

instance Element s (Label s) where
    drawElement ui s (Label p (PlainValue str)) = (translate p) <$> uiPrint ui str
    drawElement ui s (Label p (Binding b)) = (translate p) <$> uiPrint ui (b s)

uiPrint ui str = hatePrint (uiFont . base $ ui) str

-- In order to keep things simple, button cannot nest arbitrary controls
data Button s = Button Vec2 (Label s)

button :: forall s. Vec2 -> String -> AnyElement s
button pos str = AnyElement $ (Button pos (Label pos (PlainValue str) :: Label s) :: Button s)

instance Element s (Button s) where
    drawElement ui s (Button p lab) = (translate p) <$> drawElement ui s lab ++ [line (Vec2 0 0) (Vec2 10 10)]

--data ImageButton = ImageButton Hate.Sprite

{-
instance EventReceiver Button where
    accept MouseButtonState'Pressed = id
    accept _ = id
-}
