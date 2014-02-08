module Update (updateState) where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Set (member)

import Graphics.Gloss.Interface.Pure.Game

import Game
import Helpers
import State

import Debug.Trace

updateState :: Float -> State -> State
updateState dt state = appEndo (foldMap runAct (actions dt)) state
    where
    runAct (k,f) = if member k (_keysDown state) then Endo f else mempty

actions :: Float -> [(Key, State -> State)]
actions dt =
    [ (Char 'w', viewPort.vpTranslate._2 -~ 5  *dt)
    , (Char 's', viewPort.vpTranslate._2 +~ 5  *dt)
    , (Char 'a', viewPort.vpTranslate._1 +~ 5  *dt)
    , (Char 'd', viewPort.vpTranslate._1 -~ 5  *dt)
    , (Char '-', viewPort.vpScale        *~ 1-1*dt)
    , (Char '=', viewPort.vpScale        *~ 1+1*dt)
    , (Char 'r', const initialState)

    , (MouseButton LeftButton, \s -> s & selected .~ (_mouseOver s >>= newSelection (_pitch s)))
    , (MouseButton RightButton, \s -> s {- if playerInSelected then -})
    ]

newSelection :: Pitch -> (Float, Float) -> Maybe Pos
newSelection pitch xy = inPitch pitch `mfilter` Just (unCoord xy)
