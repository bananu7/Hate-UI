{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hate
import Hate.Graphics
import Hate.UI

import Control.Monad.Reader

data SampleState = SampleState {
    ui :: UI SampleState,
    counter :: Int
}

instance HasUI SampleState where
    getUI = ui

myUI = [label (Vec2 10 10) (Binding (show . counter))]

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState
    <$> (makeUI ("Arial.fnt", "Arial_0.png") myUI)
    <*> (pure 0)

sampleDraw :: DrawFn SampleState
sampleDraw s = drawUI s

sampleUpdate :: UpdateFn SampleState
sampleUpdate _ = modify $ \s -> s { counter = counter s + 1 }

config :: Config
config =
    Config
        { windowTitle = "Sample - Fonts"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
