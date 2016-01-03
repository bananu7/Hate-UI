{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hate
import Hate.Graphics
import Hate.Fonts.Loader
import Hate.Fonts
import Hate.UI

import Control.Monad.Reader

data SampleState = SampleState {
      font :: Font
    , label :: Label
    , ui :: UI
}

myPrint txt = do
    s <- ask
    return $ hatePrint (font $ s) txt

hoistUI :: Reader UI a -> Reader SampleState a
hoistUI f = ask >>= \s -> return $ runReader f (ui s)

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState 
    <$> ((,) <$> loadFontData "Arial.fnt"
             <*> loadSprite "Arial_0.png")
    <*> (pure $ Label "test label")
    <*> (UI <$> ((,) <$> loadFontData "Arial.fnt"
                     <*> loadSprite "Arial_0.png"))

sampleDraw :: DrawFn SampleState
sampleDraw = runReader $ do
    myPrint "Haters gonna Hate"
    l <- label <$> ask
    hoistUI $ drawLabel l

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
