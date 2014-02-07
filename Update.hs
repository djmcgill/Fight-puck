module Update (updateState) where

import Control.Lens
import Data.Foldable
import Data.Monoid
import Data.Set (member)

import Graphics.Gloss.Interface.Pure.Game

import Helpers
import State

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
    ]