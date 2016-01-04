{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hate
import Hate.Graphics
import Hate.UI

import Control.Monad.Reader

data SampleState = SampleState {
      label :: Label
    , ui :: UI
}

hoistUI :: Reader UI a -> Reader SampleState a
hoistUI f = ask >>= \s -> return $ runReader f (ui s)

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState 
    <$> (pure $ Label "test label")
    <*> (makeUI ("Arial.fnt", "Arial_0.png"))

sampleDraw :: DrawFn SampleState
sampleDraw = runReader $ do
    l <- label <$> ask
    hoistUI . reader $ drawElement l

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
