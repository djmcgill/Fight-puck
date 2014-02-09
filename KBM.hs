module KBM where

import Control.Applicative
import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import  Debug.Trace

import Game
import GameState

-----------------
-- Mouse click --
-----------------
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k keyState _ _) s = s & keysDown . contains k .~ (keyState == Down)
handleInput (EventMotion xy)          s = s & mouseOver ?~ invertViewPort (_viewPort s) xy
handleInput _ s = s
