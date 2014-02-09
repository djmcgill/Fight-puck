{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Update (updateGameState) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Monoid
import Data.Set (member)
import qualified Data.Map as M

import Graphics.Gloss.Interface.Pure.Game

import Game
import Helpers
import GameState

updateGameState :: Float -> GameState -> GameState
updateGameState dt gameState = appEndo (foldMap runAct (actions dt)) gameState
    where
    runAct (k,f) = if member k (_keysDown gameState) then Endo f else mempty

actions :: Float -> [(Key, GameState -> GameState)]
actions dt =
    [ (Char 'w', viewPort.vpTranslate._2 -~ 5  *dt)
    , (Char 's', viewPort.vpTranslate._2 +~ 5  *dt)
    , (Char 'a', viewPort.vpTranslate._1 +~ 5  *dt)
    , (Char 'd', viewPort.vpTranslate._1 -~ 5  *dt)
    , (Char '-', viewPort.vpScale        *~ 1-1*dt)
    , (Char '=', viewPort.vpScale        *~ 1+1*dt)
    , (Char 'r', const initialGameState)
    , (MouseButton LeftButton , setSelection      )
    , (MouseButton RightButton, moveSelectedPlayer)
    ]

setSelection :: GameState -> GameState
setSelection s = set selected (_mouseOver s >>= newSelection (_pitch s)) s
    where
    newSelection pitch xy = inPitch pitch `mfilter` Just (unCoord xy)

moveSelectedPlayer :: GameState -> GameState
moveSelectedPlayer s = maybe id movePlayerOnHex (_selected s) s

-- XXX: not finished yet. extremely temporary and messy code below
-- TODO switch this into the GameState monad and see what that looks like
movePlayerOnHex :: Pos -> GameState -> GameState
movePlayerOnHex hex s = maybe id movePlayer mPlayer s
    where
    -- could use maybe monad here
    mUID :: Maybe UID
    mUID = s ^? pitch . hexes . ix hex . _PlayerO

    mPlayer :: Maybe Player
    mPlayer = mUID >>= (\uid -> s ^? players . ix uid)

    movePlayer :: Player -> GameState -> GameState
    movePlayer p@Player{..} s' = movePlayer' 0 s'
        where
        movePlayer' :: Int -> GameState -> GameState
        movePlayer' !moved s''
            | _canMove && _speed >= moved = movePlayer' (moved+1) (s'' & pitch .~ pitch')
            where
            hex' = move _direction hex
            pitch' = case validMove hex _direction (_pitch s'') of
                Nothing -> undefined -- there was a collision here, was it with a wall or a player?
                                     -- update the player(s)
                Just p -> p

collidePlayers :: (Player, Player) -> (Player, Player)
collidePlayers = undefined

collideWall :: Player -> Player
collideWall = undefined

validMove :: Pos -> HexDir -> Pitch -> Maybe Pitch
validMove = undefined
