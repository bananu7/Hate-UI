{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI
    ( makeUI
    , drawUI
    , clickUI
    , module Hate.UI.Types
    , module Hate.UI.Controls
    )
where

import Hate.UI.Controls
import Hate.UI.Types

import Hate.Graphics
import Hate.Fonts
import Hate.Fonts.Loader
import Hate.Math

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (catMaybes)

makeUI :: (String, String) -> [AnyElement s]-> IO (UI s)
makeUI (pathFontData, pathFontSprite) elems = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) elems


drawUI :: UI -> [Element] -> [DrawRequest]
drawUI ui elements = concatMap (drawElement ui) elements

clickUI :: (HasUI s, MonadState s m) => Vec2 -> s -> m ()
clickUI p s = sequence_ . catMaybes . map (click p) $ (elements . getUI $ s)