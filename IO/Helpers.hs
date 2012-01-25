module IO.Helpers where

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import qualified Data.Map as M
import Data.Maybe (isJust)

import Game
import IO.State

data HitReturn = Hex Pos | GUI GUIElem
    deriving (Show,Eq)
data GUIElem = GUISelect
    deriving (Show,Eq)
type HitProcessor = Pos -> State -> State

---------------
-- CONSTANTS --
---------------
-- Object identifiers
hexName = Name 1
guiName = Name 2

-- GUI identifiers
selectName = Name 1

-- Colours
blue  = Color4 0.4 0.4 1   1 :: Color4 Float
green = Color4 0.4 1   0.4 1 :: Color4 Float
white = Color4 1   1   1   1 :: Color4 Float
black = Color4 0   0   0   1 :: Color4 Float
red   = Color4 1   0.4 0.4 1 :: Color4 Float
grey  = Color4 0.5 0.5 0.5 1 :: Color4 Float

hexVerts :: IO ()
hexVerts = mapM_ (vertex . uncurry Vertex2)
                 [(1,0),(0.5,h),(-0.5,h),(-1,0),(-0.5,-h),(0.5,-h)]

---------------
-- UTILITIES --
---------------

-- a bijection to convert between integers and natural numbers
toNat z = if z >= 0 then z*2     else (-z)*2 -1
toInt n = if even n then div n 2 else -(div (n+1) 2)

getSelection :: State -> Maybe Pos
getSelection (State{mode = Selection mh}) = mh
getSelection _ = Nothing

isSelection :: State -> Bool
isSelection (State{mode = Selection _}) = True
isSelection _ = False


setView :: (Float,Float) -> Float -> IO ()
setView (viewX,viewY) zoom' = do
    scale zoom' zoom' zoom'
    translate $ Vector3 viewX viewY 0

-------------------
-- HITPROCESSORS --
-------------------

setSelection :: HitProcessor
setSelection hit state = state {mode = Selection (Just hit)}
