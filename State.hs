{-# LANGUAGE TemplateHaskell #-}

module State where

import Graphics.Gloss.Interface.Pure.Game (Key)

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

import Game

data State = State {
    _pitch    :: Pitch,
    _players  :: M.Map UID Player,
    _selected :: Maybe Pos,
    _pan      :: (Float, Float),
    _zoom'    :: Float,
    _keysDown :: S.Set Key}
makeLenses ''State

initialState = State {
    _pitch    = initialPitch,
    _players  = M.empty,
    _selected = Nothing,
    _pan      = (0,0),
    _zoom'    = 15,
    _keysDown = S.empty}
