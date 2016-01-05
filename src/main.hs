{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hate
import Hate.Graphics
import Hate.UI

import Control.Monad.Reader

data SampleState = SampleState {
    ui :: UI
}

hoistUI :: Reader UI a -> Reader SampleState a
hoistUI f = ask >>= \s -> return $ runReader f (ui s)

myUI = [button (Vec2 10 10) "test label"]

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState 
    <$> (makeUI ("Arial.fnt", "Arial_0.png") myUI)

sampleDraw :: DrawFn SampleState
sampleDraw = runReader $ do
    hoistUI . reader $ drawUI

sampleUpdate :: UpdateFn SampleState
sampleUpdate _ = return ()

config :: Config
config = 
    Config
        { windowTitle = "Sample - Fonts"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
