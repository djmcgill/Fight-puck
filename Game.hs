module Game where

import Data.Map (Map)
import qualified Data.Map as M


data Pos = Pos !Int !Int
    deriving (Eq,Show,Ord)
-- The hex coordinate system, see below

type UID = Int

type Pitch = Map Pos Object

data Object = Puck | Wall | PlayerO UID | Empty

data HexDir = HLeftUp  | HLeft  | HLeftDown
            | HRightUp | HRight | HRightDown
            deriving (Eq,Enum,Ord,Show)

-- hard coding these would be faster
rotRight, rotLeft :: HexDir -> HexDir
rotRight = toEnum . (`mod` 6) .      (+) 1 . fromEnum
rotLeft  = toEnum . (`mod` 6) . subtract 1 . fromEnum

-- TODO: perhaps adjust so that right-up is `relative 1 1'?
move :: HexDir -> Pos -> Pos
move HLeftUp    = relative 0      1
move HLeft      = relative (-1)   0
move HLeftDown  = relative (-1) (-1)
move HRightUp   = relative 1      1
move HRight     = relative 1      0
move HRightDown = relative 0    (-1)

relative :: Int -> Int -> Pos -> Pos
relative x y (Pos x' y') = Pos (x+x') (y+y')

data Player = Player {
    uid :: UID}


h :: Float
h = 0.5 * sqrt 3

-- | Turn a position in the hex coordinates into standard x,y
coord :: Pos -> (Float,Float)
coord (Pos x y)
    = let x' = fromIntegral x
          y' = fromIntegral y
      in (h * (x' + 2*y'), x' * 1.5)

-- given absolute coords in the hex grid, find the hex that is under that position
-- TODO: adjust to the new coord system
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
inPitch = flip M.member

initialPitch :: Pitch
initialPitch =
    M.fromList [(Pos x y,objs)
    | x <- [-5..5], y <- [-5..5]
    , let end x = abs x == 5
    , let objs = if end y || end x then Wall else Empty]
