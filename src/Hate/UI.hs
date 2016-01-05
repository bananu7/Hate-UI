{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI
    ( makeUI
    , drawUI
    , module Hate.UI.Types
    , module Hate.UI.Controls
    )
where

import Hate.UI.Controls
import Hate.UI.Types

import Hate.Graphics
import Hate.Fonts
import Hate.Fonts.Loader

import Control.Monad.Reader
import Control.Monad.State

makeUI :: (String, String) -> [AnyElement s]-> IO (UI s)
makeUI (pathFontData, pathFontSprite) elems = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) elems

drawUI :: HasUI s => s -> [DrawRequest]
drawUI s = concatMap (drawElement s) (elements . getUI $ s)
