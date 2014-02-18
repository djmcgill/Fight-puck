{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game (Key)

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import Game

data GameState = GameState
    { _pitch     :: Pitch
    , _players   :: M.Map UID Player
    , _selected  :: Maybe Pos
    , _mouseOver :: Maybe (Float, Float)
    , _viewPort  :: ViewPort
    , _keysDown  :: S.Set Key
    , _turn      :: Int
    }
makeLenses ''GameState

initialGameState = unsafeInsertPlayer testPlayer (Pos 0 0) $ GameState
    { _pitch     = initialPitch
    , _players   = M.empty
    , _selected  = Nothing
    , _mouseOver = Nothing
    , _viewPort  = viewPortInit{viewPortScale = 10}
    , _keysDown  = S.empty
    , _turn      = 1
    }

selectedPlayer :: Traversal' GameState Player
selectedPlayer f = join $ views selectedUID $ maybe pure $ players . flip ix f

selectedUID :: Getter GameState (Maybe UID)
selectedUID = to $ \gs -> gs ^. selected >>= \pos ->
    gs ^? pitch . hexes . ix pos . _PlayerO

unsafeInsertPlayer :: Player -> Pos -> GameState -> GameState
unsafeInsertPlayer player hex
    = (pitch . hexes . at hex ?~ PlayerO (_uid player))
    . (players . at (_uid player) ?~ player)
