{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.UI
    ( makeUI
    , drawUI
    , handleEventUI
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

makeUI :: (String, String) -> AnyElement s -> IO (UI s)
makeUI (pathFontData, pathFontSprite) root = do
    fontData <- loadFontData pathFontData
    fontSprite <- loadSprite pathFontSprite
    return $ UI (UIBase (fontData, fontSprite)) root


drawUI :: HasUI s => s -> [DrawRequest]
drawUI s = drawElement (base ui) s (root ui)
    where
        ui = getUI s

handleEventUI :: (HasUI s) => UIEvent -> Effect s
handleEventUI evt = execState $ do
    ui <- getUI <$> get

    let (effOnS, root') = handleEvent evt (root ui)

    -- apply self effect
    let ui' = ui { root = root' }
    modify $ putUI ui'

    -- apply the root element effect
    modify effOnS

{-
sendUIEvent :: HasUI s => Hate.Event -> Effect s
sendUIEvent e = execState $ do
    ui <- getUI <$> get

    case e of
-}
