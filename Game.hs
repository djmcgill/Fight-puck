{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M


data Pos = Pos !Int !Int
    deriving (Eq,Show,Ord)
-- The hex coordinate system, see below

type UID = Int

data Object = Puck | PlayerO UID | Empty
    deriving (Eq, Show)
makePrisms ''Object

data Pitch = Pitch
    { _walls :: [[Pos]]
    , _hexes :: Map Pos Object
    } deriving (Eq, Show)
makeLenses ''Pitch

data HexDir = HLeftUp  | HLeft  | HLeftDown
            | HRightUp | HRight | HRightDown
            deriving (Eq,Enum,Ord,Show)

data Player = Player
    { _uid       :: UID
    , _canMove   :: Bool
    , _direction :: HexDir
    , _speed     :: Int
    } deriving (Eq, Show)


-- hard coding these would be faster
rotRight, rotLeft :: HexDir -> HexDir
rotRight = toEnum . (`mod` 6) .      (+) 1 . fromEnum
rotLeft  = toEnum . (`mod` 6) . subtract 1 . fromEnum

move :: HexDir -> Pos -> Pos
move HLeftUp    = relative 0      1
move HLeft      = relative (-1)   0
move HLeftDown  = relative (-1) (-1)
move HRightUp   = relative 1      1
move HRight     = relative 1      0
move HRightDown = relative 0    (-1)

relative :: Int -> Int -> Pos -> Pos
relative x y (Pos x' y') = Pos (x+x') (y+y')

h :: Float
h = 0.5 * sqrt 3
-----------------------------------------------
-- Pitch related things
-----------------------------------------------

-- | Turn a position in the hex coordinates into standard x,y
coord :: Pos -> (Float,Float)
coord (Pos x y)
    = let x' = fromIntegral x
          y' = fromIntegral y
      in (h * (x' + 2*y'), x' * 1.5)

getPos :: (Float, Float) -> Pitch -> Maybe Pos
getPos xy pitch = mfilter (inPitch pitch) (Just (unCoord xy))

-- given absolute coords in the hex grid, find the hex that is under that position
unCoord :: (Float, Float) -> Pos
unCoord (x, y) = Pos rx' ry'
    where
    x' = 2/3 * y
    y' = 1/3 * (sqrt(3) * x - y)
    z' = -x'-y'

    rx, ry, rz :: Int
    rx = round x'
    ry = round y'
    rz = round z'

    xDiff = abs (fromIntegral rx - x')
    yDiff = abs (fromIntegral ry - y')
    zDiff = abs (fromIntegral rz - z')

    (rx',ry') = -- ignore the biggest of the diffs
        if xDiff > yDiff && xDiff > zDiff
            then (-ry-rz, ry)
            else if yDiff > zDiff
                     then (rx, -rx-rz)
                     else (rx, ry)

inPitch :: Pitch -> Pos -> Bool
inPitch = flip M.member . _hexes

initialPitch :: Pitch
initialPitch = Pitch [] $
    M.fromList [(Pos x y, Empty)
    | x <- [-5..5], y <- [- (10 + min x 0)..10 - max x 0]]
    where width y = 20-y
