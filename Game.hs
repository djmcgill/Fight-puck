{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad
import qualified Data.Bimap as B
import Data.Map (Map)
import qualified Data.Map as M


data Pos = Pos !Int !Int
    deriving (Eq,Show,Ord)
-- The hex coordinate system, see below

type UID = Int

data Object = PlayerO UID | Empty
    deriving (Eq, Show)
makePrisms ''Object

type Walls = B.Bimap Pos Pos
-- TODO: could save this as a list of pair of hexes which you can't go from one to the other
--       it probably should be commutative for now
addWall :: Pos -> Pos -> Walls -> Walls
addWall = B.insert

wallBetween :: Pos -> Pos -> Walls -> Bool
wallBetween p1 p2 w = B.pairMember (p1,p2) w || B.pairMember (p2,p1) w

data Pitch = Pitch
    { _walls :: Walls
    , _hexes :: Map Pos Object
    } deriving (Eq, Show)
makeLenses ''Pitch

data HexDir = HLeftUp | HRightUp | HRight | HRightDown | HLeftDown | HLeft
    deriving (Eq,Enum,Ord,Show)

-- work out the required clockwise rotation in degrees to display hexdir
hexAngle :: HexDir -> Float
hexAngle dir = fromIntegral $ (fromEnum dir)*60 - 30

data Player = Player
    { _uid       :: UID
    , _canMove   :: Bool
    , _direction :: HexDir
    , _speed     :: Int
    , _upright   :: Bool
    } deriving (Eq, Show)
makeLenses ''Player

testPlayer = Player 0 True HRight 3 True


standupCost :: Int
standupCost = 2


-- hard coding these would be faster
rotRight, rotLeft :: HexDir -> HexDir
rotRight = toEnum . (`mod` 6) .      (+) 1 . fromEnum
rotLeft  = toEnum . (`mod` 6) . subtract 1 . fromEnum

move :: HexDir -> Pos -> Pos
move HLeftUp    = relative (-1)   0
move HLeft      = relative   0  (-1)
move HLeftDown  = relative   1  (-1)
move HRightUp   = relative (-1)   1
move HRight     = relative   0    1
move HRightDown = relative   1    0

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
unCoord (x, y) = nearestHex (2/3 * y) (1/3 * (sqrt(3) * x - y))

inPitch :: Pitch -> Pos -> Bool
inPitch = flip M.member . _hexes

initialPitch :: Pitch
initialPitch = Pitch B.empty $
    M.fromList [(Pos x y, Empty)
    | x <- [-5..5], y <- [- (10 + min x 0)..10 - max x 0]]

-- given hex coords, round to the nearest one
-- http://www.redblobgames.com/grids/hexagons/?#rounding
nearestHex :: Float -> Float -> Pos
nearestHex x y
    | xDiff > yDiff && xDiff > zDiff = Pos (-ry-rz)     ry
    | yDiff > zDiff                  = Pos      rx (-rx-rz)
    | otherwise                      = Pos      rx      ry
    where
    z = -x-y

    rx, ry, rz :: Int
    rx = round x
    ry = round y
    rz = round z

    xDiff = abs (fromIntegral rx - x)
    yDiff = abs (fromIntegral ry - y)
    zDiff = abs (fromIntegral rz - z)