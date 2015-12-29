{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Hate.UI where

import Hate.Graphics
import qualified Hate.Events.Types as Hate

{-
data UIEvent =
      ClickEvent GLFW.MouseButton
    | MouseOverEvent
    | MouseOffEvent 

class EventReceiver r where
    accept :: UIEvent -> r -> r

class (EventReceiver a, Show a) => Element a where

-}

class Element a where
    drawElement :: a -> [DrawRequest]

data ElementNode = forall a. Element a => ElementNode {
      value :: a
    , children :: [ElementNode]
}

instance Element ElementNode where
    drawElement (ElementNode val cs) = drawElement val ++ concatMap drawElement cs

-- Here come the actual controls
data Label = Label String

-- In order to keep things simple, button cannot nest arbitrary controls
data Button = Button Label
--data ImageButton = ImageButton Hate.Sprite

{-
instance EventReceiver Button where
    accept MouseButtonState'Pressed = id
    accept _ = id
-}
