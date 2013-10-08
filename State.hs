{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M

import Game

data State = State {
    _pitch       :: Pitch,
    _players     :: Map UID Player,
    _selected    :: Maybe Pos,
    _pan         :: (Float, Float),
    _scaleFactor :: Float}
makeLenses ''State

initialState = State {
    _pitch       = initialPitch,
    _players     = M.empty,
    _selected    = Nothing,
    _pan         = (0,0),
    _scaleFactor = 15}

updateState = id -- placeholder