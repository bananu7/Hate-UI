{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Hate.UI.Types where

import Hate.Fonts
import Hate.Graphics
import Hate.Math

import Control.Monad.State

data UIBase = UIBase {
    uiFont :: Font
}

-- The s type is the ultimate type of state the UI can see
-- in other words, it can do only operations on s, and its
-- subcomponents can do operations on subparts on s
data UI s = UI {
    base :: UIBase,
    root :: AnyElement s
}

-- |This is an equivalent of the state monad, but tailored for the UI.
-- We'll see how useful this approach is.
class HasUI s where
    getUI :: s -> UI s
    putUI :: UI s -> s -> s

-- |Represents a binding to a type of value a inside of state s
data Binding s a = PlainValue a | Binding (s -> a) deriving Functor

type Effect s = s -> s
type SelfEffect s a = (Effect s, a)

class Element s a where
    drawElement :: UIBase -> s -> a -> [DrawRequest]

    handleEvent  :: UIEvent -> a -> SelfEffect s a
    handleEvent _ x = (id, x)

data AnyElement s = forall e. Element s e => AnyElement e
instance Element s (AnyElement s) where
    drawElement ub s (AnyElement e) = drawElement ub s e

    handleEvent evt (AnyElement (e :: e)) = (sE, AnyElement selfE)
        where
            (sE, selfE) = (handleEvent evt e :: SelfEffect s e)

-- Events
data MouseButton = MouseButtonLeft | MouseButtonMiddle | MouseButtonRight

data UIEvent = 
      UIEvent'MouseDown MouseButton Vec2 
    | UIEvent'MouseMove Vec2
