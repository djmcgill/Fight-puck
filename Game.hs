module Game where

import Data.Map (Map)
import qualified Data.Map as M


data Pos = Pos !Int !Int
    deriving (Eq,Show,Ord)
-- The hex coordinate system, see below

type UID = Int

type Pitch = Map Pos Object

data Object = Puck | Wall | PlayerO UID | Empty

data HexDir = LeftUp   | Up   | RightUp
            | LeftDown | Down | RightDown
            deriving (Eq,Enum,Ord,Show)

-- hard coding these would be faster
rotRight, rotLeft :: HexDir -> HexDir
rotRight = toEnum . (`mod` 6) .      (+) 1 . fromEnum
rotLeft  = toEnum . (`mod` 6) . subtract 1 . fromEnum

move :: HexDir -> Pos -> Pos
move LeftUp    = relative (-1) 1
move Up        = relative   0  1
move RightUp   = relative   1  0
move LeftDown  = relative (-1) 0
move Down      = relative   0  (-1)
move RightDown = relative   1  (-1)

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
 _________
|    r   /
|h      /
|      /
|_____/
-}
h :: Float
h = cos (pi / 6)

-- | Turn a position in the hex coordinates into standard x,y
coord :: Pos -> (Float,Float)
coord (Pos x y)
    = let x' = fromIntegral x
          y' = fromIntegral y
      in (x'* 2*h*cos(pi/6), x'* 2*h*sin(pi/6)
                           + y'* 2*h)

-- | > pairs [1,2,3,4] = [(1,2),(2,3),(3,4)]
pairs :: [a] -> [(a,a)]
pairs (a:a':as) = (a,a') : pairs (a':as)
pairs _ = []

initialPitch :: Pitch
initialPitch =
    M.fromList [(Pos x y,objs)
    | x <- [-5..5], y <- [-5..5]
    , let end x = abs x == 5
    , let objs = if end y || end x then Wall else Empty]
