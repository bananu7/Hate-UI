{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Hate.UI where

import Hate.Graphics
import qualified Hate.Events.Types as Hate
import Hate.Fonts
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

data UI = UI { 
    uiFont :: Font
    --, rootElement :: ElementNode 
}

{-
class Element a where
    drawElement :: MonadReader UI m => a -> m [DrawRequest]

instance Element ElementNode where
    drawElement (ElementNode val cs) = drawElement val ++ concatMap drawElement cs
-}

type WithUI a = forall m. MonadState UI m => m a
type WithUIRead a = forall m. MonadReader UI m => m a

-- Here come the actual controls
data Label = Label String

drawLabel :: Label -> WithUIRead [DrawRequest]
drawLabel (Label str) = ask >>= \ui -> return $ hatePrint (uiFont ui) str

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
