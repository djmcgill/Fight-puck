{-# LANGUAGE RecordWildCards #-}
module Draw where

import Control.Lens
import Control.Monad (forM_)
import qualified Data.Map as M

import Graphics.Gloss

import Game
import Helpers
import State

drawState :: State -> Picture
drawState (State{..}) = Scale _scaleFactor _scaleFactor $ uncurry Translate _pan $
        Pictures [drawPitch _pitch, maybe Blank drawSelection _selected]

drawPitch :: Pitch -> Picture
drawPitch = Pictures . map eachObject . M.toList
    where
    eachObject (pos,object) = translatePos pos $ Pictures [hex, drawObject object, boundary]
    hex      = Color white $ Polygon hexVerts
    boundary = Color black $ Line    hexVerts

drawObject :: Object -> Picture
drawObject Wall = Color blue (Polygon hexVerts)
drawObject _    = Blank

drawSelection :: Pos -> Picture
drawSelection pos = translatePos pos $ Color red $ Line hexVerts

drawPath :: [Pos] -> Picture
drawPath path = Color black $ Line $ map coord path
    -- note here we have
        -- Line [v0,v1,v2,...]
    -- might need
        -- Pictures [Line [v0,v1], Line [v1,v2], ...]
    -- instead