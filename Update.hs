{-# LANGUAGE BangPatterns, RecordWildCards, LambdaCase, MultiWayIf #-}

module Update (updateGameState) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Maybe
import Data.Monoid
import Data.Set (member)
import qualified Data.Map as M

import Graphics.Gloss.Interface.Pure.Game

import Game
import Helpers
import GameState

updateGameState :: Float -> GameState -> GameState
updateGameState dt gameState = appEndo (F.foldMap runAct (actions dt)) gameState
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
setSelection s = s & selected .~ exists (unCoord <$> _mouseOver s)
    where
    exists = mfilter (inPitch (_pitch s))

moveSelectedPlayer :: GameState -> GameState
moveSelectedPlayer s = fromMaybe s $ do
    hex  <- _selected s
    xy   <- _mouseOver s
    hex' <- getPos xy (_pitch s)
    guard $ hex == hex'
    return (movePlayerFromHex hex s)

-- note that this function will crash if the uids exist in the board that aren't
-- in the map of players
movePlayerFromHex :: Pos -> GameState -> GameState
movePlayerFromHex initHex s = flip (maybe s) (s ^? pitch . hexes . ix initHex . _PlayerO) $ \uid ->
    let movePlayer :: Int -> State GameState ()
        movePlayer !distMoved = use (players . at uid) >>= \(Just p@Player{..}) -> if
            | _canMove && _upright && _speed >= distMoved -> do
                pitch' <- use pitch
                let Just hex = uidLocation uid pitch'
                    hex'     = move _direction hex
                selected .= Nothing

                -- what is in front of the player?
                case pitch' ^. hexes . at hex' of
                    -- a wall
                    _ | wallBetween hex hex' (_walls pitch') -> onPlayer p %= collideWall
                    -- the hex doesn't exist
                    Nothing -> onPlayer p %= collideWall
                    -- another player
                    Just (PlayerO uid2) -> do
                        Just p2 <- use (players . at uid2)
                        let (p', p2') = collidePlayers p p2
                        onPlayer p  .= p'
                        onPlayer p2 .= p2'
                    -- an empty hex
                    Just Empty -> do
                        pitch . hexes . ix hex' .= PlayerO uid
                        pitch . hexes . ix hex  .= Empty
                movePlayer (distMoved+1)

            -- stand up
            | _canMove && not _upright && _speed >= (distMoved + standupCost) -> do
                players . ix uid . upright .= True
                movePlayer (distMoved+standupCost)

            -- finished move
            | _canMove -> players . ix uid . canMove .= False

            -- can't move
            | otherwise -> return ()
    in execState (movePlayer 0) s

collidePlayers :: Player -> Player -> (Player, Player)
collidePlayers p p2 = (fall p, fall p2)

fall :: Player -> Player
fall = upright .~ False

collideWall :: Player -> Player
collideWall = fall

uidLocation :: UID -> Pitch -> Maybe Pos
uidLocation uid = fmap fst . listToMaybe . filter ((PlayerO uid ==) . snd) . M.toList . _hexes
-- traversed.ifiltered?


onPlayer p = players.ix (_uid p)