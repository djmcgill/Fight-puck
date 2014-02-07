module Game where

import Data.Map (Map)
import qualified Data.Map as M


data Pos = Pos !Int !Int
    deriving (Eq,Show,Ord)
-- The hex coordinate system, see below

type UID = Int

type Pitch = Map Pos Object

data Object = Puck | Wall | PlayerO UID | Empty

data HexDir = HLeftUp   | HUp   | HRightUp
            | HLeftDown | HDown | HRightDown
            deriving (Eq,Enum,Ord,Show)

-- hard coding these would be faster
rotRight, rotLeft :: HexDir -> HexDir
rotRight = toEnum . (`mod` 6) .      (+) 1 . fromEnum
rotLeft  = toEnum . (`mod` 6) . subtract 1 . fromEnum

move :: HexDir -> Pos -> Pos
move HLeftUp    = relative (-1) 1
move HUp        = relative   0  1
move HRightUp   = relative   1  0
move HLeftDown  = relative (-1) 0
move HDown      = relative   0  (-1)
move HRightDown = relative   1  (-1)

relative :: Int -> Int -> Pos -> Pos
relative x y (Pos x' y') = Pos (x+x') (y+y')

data Player = Player {
    uid :: UID}

{-
see the move function for an idea of the axes. The very middle is (0,0)
     _   _   _   _   _
    / \_/a\_/b\_/c\_/ \_
    \_/d\_/e\_/f\_/ \_/ \
    /g\_/h\_/i\_/ \_/ \_/
    \_/ \_/j\_/ \_/ \_/
          \_/

the hex-x-axis is the / direcition
the hex-y-axis is the same as the normal y axis, the | direction
movement in the \ direction is a combination of the other two

so: if e = ( 0, 0) then
       a = (-1, 1)
       b = ( 1, 0)
       c = ( 3,-1)
       d = (-2, 1)
       f = ( 2,-1)
       g = (-3, 1)
       h = (-1, 0)
       i = ( 1,-1)
       j = ( 0,-1)
-}



-- if the maximum radius (r) of the hex is 1, the minimum radius (h) is cos(pi / 6)
{-
the picture below shows the bottom corner of a hex
       v--- = 0.5
 _________
|    r   /
|h      /
|      /
|_____/
-}
h :: Float
h = 0.5 * sqrt 3

-- | Turn a position in the hex coordinates into standard x,y
coord :: Pos -> (Float,Float)
coord (Pos x y)
    = let x' = fromIntegral x
          y' = fromIntegral y
      in (x' * 1.5, h * (x' + 2*y'))

-- given absolute coords in the hex grid, find the hex that is under that position
unCoord :: (Float, Float) -> Pos
unCoord (x, y) = Pos rx' ry'
    where
    x' = (2/3) * x
    y' = (1/3) * sqrt 3 * y - (1/3) * x
    z' = -x'-y'

    rx, ry, rz :: Int
    rx = round x'
    ry = round y'
    rz = round z'

    xDiff = abs (fromIntegral rx - x')
    yDiff = abs (fromIntegral ry - y')
    zDiff = abs (fromIntegral rz - z')

    (rx',ry') =
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
