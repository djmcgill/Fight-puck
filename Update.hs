{-# LANGUAGE BangPatterns, RecordWildCards, LambdaCase #-}

module Update (updateGameState) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Set (member)
import qualified Data.Map as M

import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

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
moveSelectedPlayer s =  maybe id movePlayerFromHex (_selected s) s

data Collision = WallC | PlayerC UID

-- XXX: not finished yet. extremely temporary and messy code below
-- TODO need to check that the mouse is over the selected player too
movePlayerFromHex :: Pos -> GameState -> GameState
movePlayerFromHex initHex s = fromMaybe s $ do
    uid <- s ^? pitch . hexes . ix initHex . _PlayerO
    p@Player{..} <- s ^? players . ix uid

    let movePlayer :: Int -> State GameState ()
        movePlayer !moved
            | _canMove && _upright && _speed >= moved = do
                collision <- zoom pitch updatePitch
                case collision of
                    Just WallC          -> onPlayer p %= collideWall
                    Just (PlayerC uid2) -> case s ^? players . ix uid2 of
                        Nothing -> return ()
                        Just p2 -> do
                            let (p', p2') = collidePlayers p p2
                            onPlayer p  .= p'
                            onPlayer p2 .= p2'
                    Nothing -> return ()
                movePlayer (moved+1)
            | _canMove && not _upright && _speed >= (moved + standupCost) = do
                onPlayer p . upright .= True
                movePlayer (moved+standupCost)
            | _canMove = onPlayer p . canMove .= False
            | otherwise = return ()

            where
            updatePitch :: State Pitch (Maybe Collision)
            updatePitch = do
                pitch' <- get
                let hex = fromMaybe (error $ "validMoved called on non-existent uid: " ++ show uid) $
                        uidLocation uid pitch'
                    hex' = move _direction hex

                    validMove :: Pitch -> Either Collision Pitch
                    validMove pitch''
                        | wallBetween hex hex' (_walls pitch'')  = Left WallC
                        | otherwise = case pitch'' ^. hexes . at hex' of
                            Nothing             -> Left WallC
                            Just (PlayerO uid2) -> Left (PlayerC uid2)
                            Just Empty          -> Right (swapPlayer pitch'')
                        where
                        swapPlayer = (hexes . ix hex' .~ PlayerO uid) . (hexes . ix hex .~ Empty)
                case validMove pitch' of
                    Left c  -> return (Just c)
                    Right p -> put p >> return Nothing




    return $ execState (movePlayer 0) s

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