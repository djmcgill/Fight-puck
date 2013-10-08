module Helpers where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe (isJust)
import Graphics.Gloss

import Game
import State

hexVerts :: Path
hexVerts = [(1,0),(0.5,h),(-0.5,h),(-1,0),(-0.5,-h),(0.5,-h)]

translatePos = uncurry Translate . coord
