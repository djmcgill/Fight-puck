module IO.State where

import Game
import qualified Data.Map as M
import Graphics.UI.GLUT (MouseButton(..), Position(..))

data State = State
    { pitch   :: Pitch
    , players :: Players
    , mode    :: Mode
    , pan     :: (Float, Float)
    , zoom    :: Float
    , mouse   :: Maybe MouseButton
    , mPos    :: Maybe Position
    }

initialState = State
    { pitch   = initialPitch
    , players = []
    , mode    = Selection Nothing
    , pan     = (0,0)
    , zoom    = 0.1
    , mouse   = Nothing
    , mPos    = Nothing
    }

data Mode = Selection (Maybe Pos)
    deriving (Show,Eq)

-- the idea is that this function moves all the moving things and so on
updateState :: State -> State
updateState = id
