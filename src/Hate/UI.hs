{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

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

data UI = UI { 
    base :: UIBase,
    elements :: [AnyElement]
    --, rootElement :: ElementNode 
}

makeUI :: (String, String) -> [AnyElement]-> IO UI
makeUI (pathFontData, pathFontSprite) elems = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) elems

drawUI :: UI -> [DrawRequest]
drawUI ui = concatMap (drawElement ui) $ elements ui

class Element a where
    drawElement :: UI -> a -> [DrawRequest]

data AnyElement = forall e. Element e => AnyElement e
instance Element AnyElement where
    drawElement ui (AnyElement e) = drawElement ui e

-- Here come the actual controls
data Label = Label String

label :: String -> AnyElement
label str = AnyElement $ Label str

instance Element Label where
    drawElement ui (Label str) = hatePrint (uiFont . base $ ui) str

-- In order to keep things simple, button cannot nest arbitrary controls
data Button = Button Label

button :: String -> AnyElement
button str = AnyElement $ Button (Label str)

instance Element Button where
    drawElement ui (Button lab) = drawElement ui lab ++ [rectangle (Vec2 10 10)]

--data ImageButton = ImageButton Hate.Sprite

{-
instance EventReceiver Button where
    accept MouseButtonState'Pressed = id
    accept _ = id
-}
