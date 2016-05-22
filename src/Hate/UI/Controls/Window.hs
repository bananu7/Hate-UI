{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hate.UI.Controls.Window
    ( window
    , Window -- TODO TEMP BECAUSE AMBIGUOUS
    )
where

import Hate.UI.Types
import Hate.UI.Controls.Button
import Hate.UI.Util

import Hate.Graphics hiding (size)
import Hate.Math hiding (position)
import Control.Lens

-- This is an example; it should probably be moved to a proper example program at some
-- point, and replaced with something genuinely useful.
data Window s = Window {
    _windowPosition :: Vec2,
    _windowSize :: Vec2,
    _WindowAddBtn :: Button (Window s),
    _WindowDummies :: [Button ()]
}
makeLenses ''Window
makeFields ''Window

instance Element s (Window s) where
    drawElement ub s w = translate (w ^. position) <$> (
        (box (Vec2 0 0) (w ^. size)) ++
        (concatMap (drawElement ub ()) (w ^.windowDummies)) ++
        (drawElement ub w (w ^. windowAddBtn))
        )

    handleEvent evt w@(Window pos sz add dummies) = (id, self')
        where
            (winE, addBtn') :: SelfEffect (Window s) (Button (Window s)) = handleEvent (relativeEvt evt) add
            -- Updating the button (component) currently must be done manually
            -- Probably a helper would be apt
            self' = winE $ w & windowAddBtn .~ addBtn'

            relativeEvt (UIEvent'MouseDown btn mp) = UIEvent'MouseDown btn (mp - pos)
            relativeEvt (UIEvent'MouseMove mp) = UIEvent'MouseMove (mp - pos)
            relativeEvt x = x

window :: forall s. Vec2 -> Vec2 -> Int -> Window s
window pos sz n = Window pos sz addBtn children
    where
        children = map newDummy [1..n]
        addBtn = button (Vec2 10 0) (Vec2 100 20) ("add new dummy!") (addDummy)
        addDummy = windowDummies %~ \dums -> dums ++ [newDummy $ length dums]
        newDummy n = button (Vec2 10 (fromIntegral n * 25)) (Vec2 100 20) ("button " ++ show n) id
