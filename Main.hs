module Main where

import Graphics.Gloss

import Game
import Draw
import Helpers
import KBM
import State
import Update

main = play display black fps initialState drawState handleInput updateState
    where
    fps = 30
    display = InWindow "gloss test" (width,height) (0,0)
    width  = 640
    height = 480
