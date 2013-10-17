module KBM where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import State

handleInput :: Event -> State -> State
handleInput (EventKey k keyState _ _) = keysDown . contains k .~ (keyState == Down)
handleInput _ = id
