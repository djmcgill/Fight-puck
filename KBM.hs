module KBM where

import Control.Applicative
import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import  Debug.Trace

import Game
import State

-----------------
-- Mouse click --
-----------------
handleInput :: Event -> State -> State
handleInput (EventKey k keyState _ _) s = s & keysDown . contains k .~ (keyState == Down)
handleInput (EventMotion xy)          s = s & selected .~ getPos (invertViewPort (s^.viewPort) xy) (s^.pitch)
handleInput _ s = s

-- note this can be done with Control.Lens.Reified.Fold:
-- runFold $ (,) <$> Fold viewPort <*> Fold selected

