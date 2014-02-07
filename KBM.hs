module KBM where

import Control.Applicative
import Control.Lens
import Control.Lens.Fold
import Control.Lens.Reified
import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Game
import State

-----------------
-- Mouse click --
-----------------
handleInput :: Event -> State -> State
handleInput (EventKey k keyState _ _) s = s & keysDown . contains k .~ (keyState == Down)
handleInput (EventMotion pos)         s = s & selected .~ newSelection pos (s^.viewPort) (s^.pitch)
handleInput _ s = s

newSelection :: (Float, Float) -> ViewPort -> Pitch -> Maybe Pos
newSelection xy vp pitch = inPitch pitch `mfilter` Just pos
	where pos = unCoord (invertViewPort vp xy)


-- note this can be done with Control.Lens.Reified.Fold:
-- runFold $ (,) <$> Fold viewPort <*> Fold selected

