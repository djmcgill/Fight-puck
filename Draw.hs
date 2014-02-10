{-# LANGUAGE RecordWildCards #-}
module Draw where

import Control.Lens
import qualified Data.Map as M

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Game
import Helpers
import GameState

drawGameState :: GameState -> Picture
drawGameState (GameState{..}) = applyViewPortToPicture _viewPort $
        Pictures [drawPitch _pitch, maybe Blank drawSelection _selected]

drawPitch :: Pitch -> Picture
drawPitch (Pitch{..}) = Pictures [drawHexes _hexes, drawWalls _walls]

drawHexes :: M.Map Pos Object -> Picture
drawHexes = Pictures . map eachObject . M.toList
    where
    eachObject (pos,object) = translatePos pos $ Pictures [hex, drawObject object, boundary]
    hex      = Color white $ Polygon hexVerts
    boundary = Color black $ Line    hexVerts

drawWalls :: Walls -> Picture
drawWalls _ = Blank -- for each pair in the bimap, draw a line between them

drawObject :: Object -> Picture

-- drawObject Puck          = Color black (Circle 0.3)
drawObject (PlayerO _) = Color black (Circle 0.3) -- TODO: = drawPlayer (lookup uid)
drawObject _ = Blank

drawPlayer :: Player -> Picture
drawPlayer (Player {..}) = Color (if _canMove then blue else light blue) (Circle 0.4)
-- TODO: obv this is pretty inadequate

drawSelection :: Pos -> Picture
drawSelection pos = translatePos pos $ Color red $ Line hexVerts

drawPath :: [Pos] -> Picture
drawPath = Color black . Line . map coord
