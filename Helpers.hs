{-# LANGUAGE TemplateHaskell #-}

module Helpers where

import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Game

hexVerts :: Path
hexVerts = [(0,1),(h,0.5),(h,-0.5),(0,-1),(-h,-0.5),(-h,0.5),(0,1)]

translatePos = uncurry Translate . coord

-- Lenses for Viewports
makeLensesFor
	[ ("viewPortTranslate", "vpTranslate")
	, ("viewPortRotate"   , "vpRptate"   )
	, ("viewPortScale"    , "vpScale"    )]
	''ViewPort