{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hate
import Hate.Graphics
import Hate.UI

import Control.Monad.Reader

data SampleState = SampleState {
    ui :: UI SampleState,
    counter :: Int,
    mousePos :: Vec2
}

instance HasUI SampleState where
    getUI = ui

myUI = [
    label (Vec2 10 10) (Binding (show . counter)),
    button (Vec2 10 40) "button" (\s -> s { counter = 0 })
    ]

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState
    <$> (makeUI ("Arial.fnt", "Arial_0.png") myUI)
    <*> (pure 0)
    <*> (pure $ Vec2 0 0)

sampleDraw :: DrawFn SampleState
sampleDraw s = drawUI s

processEvent :: MonadState SampleState m => Event -> m ()
processEvent (EventCursorPos x y) = modify $ \s -> s { mousePos = Vec2 x y }

-- GLFW.MouseButtonState'Released 
processEvent (EventMouseButton _ _ _) = do
    mp <- gets mousePos
    s <- get
    clickUI mp s
    
processEvent _ = return ()

sampleUpdate :: UpdateFn SampleState
sampleUpdate evts = do
    mapM_ processEvent evts
    modify $ \s -> s { counter = counter s + 1 }

config :: Config
config =
    Config
        { windowTitle = "Sample - Fonts"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
