{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hate.UI.Controls.Window
    ( window
    , Window -- TODO TEMP BECAUSE AMBIGUOUS
    )
where

import Hate.UI.Types
import Hate.UI.Controls.Button
import Hate.UI.Util

import Hate.Graphics
import Hate.Math

data Window s = Window {
    windowPos :: Vec2,
    windowSz :: Vec2,
    windowAddBtn :: Button (Window s),
    windowDummies :: [Button ()]
}

instance Element s (Window s) where
    drawElement ub s w = translate (windowPos w) <$> (
        (box (Vec2 0 0) (windowSz w)) ++
        (concatMap (drawElement ub ()) (windowDummies w)) ++
        (drawElement ub w (windowAddBtn w))
        )

    click mp w@(Window pos sz add dummies) = (id, self')
        where
            (winE, addBtn') :: SelfEffect (Window s) (Button (Window s)) = click (mp - pos) add
            -- Updating the button (component) currently must be done manually
            -- Probably a helper would be apt
            self' = winE $ w { windowAddBtn = addBtn' }

window :: forall s. Vec2 -> Vec2 -> Int -> Window s
window pos sz n = Window pos sz addBtn children
    where
        children = map newDummy [1..n]
        addBtn = button (Vec2 10 0) (Vec2 100 20) ("add new dummy!") (addDummy)
        addDummy w = w { windowDummies = windowDummies w ++ [newDummy (length $ windowDummies w)]}
        newDummy n = button (Vec2 10 (fromIntegral n * 25)) (Vec2 100 20) ("button " ++ show n) id
