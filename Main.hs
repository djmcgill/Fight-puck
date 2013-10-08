module Main where

import Graphics.Gloss

import Game
import Draw
import Helpers
import KBM
import State

main = play display black fps initialState drawState handleInput stepWorld
    where
    stepWorld = const id
    fps = 30
    display = InWindow "gloss test" (width,height) (0,0)
    width  = 640
    height = 480
