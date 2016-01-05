module Hate.UI.Util where

import Hate.UI.Types

import Hate.Fonts

uiPrint ui str = hatePrint (uiFont . base $ ui) str
