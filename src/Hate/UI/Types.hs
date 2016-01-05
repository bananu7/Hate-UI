{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Hate.UI.Types where

import Hate.Fonts
import Hate.Graphics

data UIBase = UIBase {
    uiFont :: Font
}

data UI s = UI {
    base :: UIBase,
    elements :: [AnyElement s]
}


-- |This is an equivalent of the state monad, but tailored for the UI.
-- We'll see how useful this approach is.
class HasUI s where
    getUI :: s -> UI s

-- |Represents a binding to a type of value a inside of state s
data Binding s a = PlainValue a | Binding (s -> a)

class Element s a where
    drawElement :: HasUI s => s -> a -> [DrawRequest]

data AnyElement s = forall e. Element s e => AnyElement e
instance Element s (AnyElement s) where
    drawElement s (AnyElement e) = drawElement s e

