{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hate
import Hate.Graphics
import Hate.Fonts.Loader
import Hate.Fonts
import Hate.UI

import Control.Monad.Reader

data FontConfig = FontConfig {
    fontSprite :: Sprite,
    fontData :: Font
}

data SampleState = SampleState {
    fontConfig :: FontConfig
}

myPrint txt = do
    s <- ask
    return $ hatePrint (fontData . fontConfig $ s) (fontSprite . fontConfig $ s) txt


sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$>
    (FontConfig <$> loadSprite "Arial_0.png"
                <*> loadFont "Arial.fnt")

sampleDraw :: DrawFn SampleState
sampleDraw = runReader $ do
    myPrint "Haters gonna Hate"

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
