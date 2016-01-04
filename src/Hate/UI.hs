{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Hate.UI where

import Hate.Graphics
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

makeUI :: (String, String) -> IO UI
makeUI (pathFontData, pathFontSprite) = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) []

drawUI :: UI -> [DrawRequest]
drawUI ui = concatMap (drawElement ui) $ elements ui

class Element a where
    drawElement :: UI -> a -> [DrawRequest]

data AnyElement = forall e. Element e => AnyElement e
instance Element AnyElement where
    drawElement ui (AnyElement e) = drawElement ui e

{-
instance Element ElementNode where
    drawElement (ElementNode val cs) = drawElement val ++ concatMap drawElement cs
-}

-- Here come the actual controls
data Label = Label String

instance Element Label where
    drawElement ui (Label str) = hatePrint (uiFont . base $ ui) str

--instance Element Label where
--    drawElement (Label str) = hatePrint .. .. str

-- In order to keep things simple, button cannot nest arbitrary controls
data Button = Button Label
--instance Element Button where



--data ImageButton = ImageButton Hate.Sprite

{-
instance EventReceiver Button where
    accept MouseButtonState'Pressed = id
    accept _ = id
-}
