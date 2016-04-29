{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI.Types where

import Hate.Fonts
import Hate.Graphics
import Hate.Math

import Control.Monad.State

data UIBase = UIBase {
    uiFont :: Font
}

data UI s = UI {
    base :: UIBase
}


-- |This is an equivalent of the state monad, but tailored for the UI.
-- We'll see how useful this approach is.
class HasUI s where
    getUI :: s -> UI s

-- |Represents a binding to a type of value a inside of state s
data Binding s a = PlainValue a | Binding (s -> a)

type Effect s = forall m. (MonadState s m) => m ()
type SelfEffect s a = Maybe (Effect s, a -> a)

class Element s a where
    drawElement :: UI -> a -> [DrawRequest]

    click :: Vec2 -> a -> SelfEffect s a
    click _ _ = Nothing

{-
class EventReceiver s a where
    receive :: Event -> State a (Effect s)
-}

data AnyElement s = forall e. Element s e => AnyElement e
instance Element s (AnyElement s) where
    drawElement s (AnyElement e) = drawElement s e
    click mp (AnyElement (e :: e)) = case (click mp e :: SelfEffect s e) of
        Nothing -> Nothing


