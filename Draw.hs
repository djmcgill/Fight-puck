{-# LANGUAGE RecordWildCards #-}
module Draw where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import Data.Maybe


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Game
import Helpers
import GameState

drawGameState :: GameState -> Picture
drawGameState gs@GameState{..} = flip runReader gs $ applyViewPortToPicture _viewPort . Pictures <$>
        sequence [drawPitch _pitch, maybe (return Blank) drawSelection _selected]

drawPitch :: Pitch -> Reader GameState Picture
drawPitch (Pitch{..}) = Pictures <$> sequence [drawHexes _hexes, drawWalls _walls]

drawHexes :: M.Map Pos Object -> Reader GameState Picture
drawHexes = fmap Pictures . mapM eachObject . M.toList
    where
    eachObject (pos,object) = translatePos pos . Pictures <$> sequence [hex, drawObject object, boundary]
    hex      = return $ Color white $ Polygon hexVerts
    boundary = return $ Color black $ Line    hexVerts

drawWalls :: Walls -> Reader GameState Picture
drawWalls _ = return Blank -- for each pair in the bimap, draw a line between them

drawObject :: Object -> Reader GameState Picture

-- drawObject Puck          = return $ Color black (Circle 0.3)
drawObject (PlayerO uid) = magnify (players . at uid) ask >>= drawPlayer . fromJust
drawObject _ = return $ Blank

drawPlayer :: Player -> Reader GameState Picture
drawPlayer (Player {..}) = return
    . Color (if _canMove then blue else light blue)
    . Rotate (hexAngle _direction)
    . scale 0.4 0.4
    $ triangle

triangle :: Picture
triangle = Polygon [(0,1),(x,-0.5),(-x,-0.5)]
	where x = sin(2*pi/3)

drawSelection :: Pos -> Reader GameState Picture
drawSelection pos = return $ translatePos pos $ Color red $ Line hexVerts

drawPath :: [Pos] -> Reader GameState Picture
drawPath = return . Color black . Line . map coord
