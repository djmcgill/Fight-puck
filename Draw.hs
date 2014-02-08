{-# LANGUAGE RecordWildCards #-}
module Draw where

import Control.Lens
import qualified Data.Map as M

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Game
import Helpers
import State

drawState :: State -> Picture
drawState (State{..}) = applyViewPortToPicture _viewPort $
        Pictures [drawPitch _pitch, maybe Blank drawSelection _selected]

drawPitch :: Pitch -> Picture
drawPitch (Pitch{..}) = Pictures [drawHexes _hexes, Pictures (map drawWall _walls)]

drawHexes :: M.Map Pos Object -> Picture
drawHexes = Pictures . map eachObject . M.toList
    where
    eachObject (pos,object) = translatePos pos $ Pictures [hex, drawObject object, boundary]
    hex      = Color white $ Polygon hexVerts
    boundary = Color black $ Line    hexVerts

drawWall :: [Pos] -> Picture
drawWall _ = Blank

drawObject :: Object -> Picture
drawObject Puck          = Color black (Circle 0.3)
drawObject _             = Blank

drawPlayer :: Player -> Picture
drawPlayer (Player {..}) = Color (if _canMove then blue else light blue) (Circle 0.4)

drawSelection :: Pos -> Picture
drawSelection pos = translatePos pos $ Color red $ Line hexVerts

drawPath :: [Pos] -> Picture
drawPath = Color black . Line . map coord
