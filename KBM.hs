module KBM where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game (Pos(..))
import State
import Helpers
import Draw

-----------------
-- Mouse click --
-----------------
handleInput :: Event -> State -> State
handleInput (EventKey (Char 'w') Down _ _) = pan %~ panUp
handleInput (EventKey (Char 's') Down _ _) = pan %~ panDown
handleInput (EventKey (Char 'a') Down _ _) = pan %~ panLeft
handleInput (EventKey (Char 'd') Down _ _) = pan %~ panRight
handleInput (EventKey (Char '-') Down _ _) = scaleFactor *~ 0.9
handleInput (EventKey (Char '=') Down _ _) = scaleFactor *~ 1.1
handleInput (EventKey (Char '.') Down _ _) = const initialState
handleInput _                              = id
{-
keyCallback ref (MouseButton LeftButton) Down _ mouse = do
-- change which hex is selected
    modifyMVar_ ref $ \state' -> do
        let state = state'{mouse = Just LeftButton}
        hit <- getHit state mouse
        return $ case hit of
            Just (Hex hex) | isSelection state -> setSelection hex state
            Just (GUI GUISelect) -> state {mode = Selection Nothing}
            _ -> state
    postRedisplay Nothing
keyCallback ref (MouseButton RightButton) Down _ mouse = do
-- move the selected hex
    modifyMVar_ ref $ \state' ->
        return $ state'{mouse = Just RightButton}
keyCallback ref (MouseButton MiddleButton) Down _ mouse = do
    modifyMVar_ ref $ \state ->
        return $ state {mouse = Just MiddleButton, mPos = Just mouse}
    addTimerCallback 10 $ middlePan ref
keyCallback ref (MouseButton _) Up _ _ =
    mouseUp ref
-}
keyCallback _ _ _ _ _ _ = return ()

-- TODO: change this to lens fst and snd
panUp, panDown, panLeft, panRight :: (Float, Float) -> (Float, Float)
panUp    (x,y) = (x  ,y-1)
panDown  (x,y) = (x  ,y+1)
panLeft  (x,y) = (x+1,y  )
panRight (x,y) = (x-1,y  )
{-
getHit :: State -> Position -> IO (Maybe HitReturn)
getHit state (Position x y) = undefined {- do
    viewport'@(_, Size width height) <- get viewport
    (_, mhits) <- getHitRecords 512 $ withName (Name 0) $ do
        matrixMode $= Projection
        loadIdentity
        pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y)
                   (2, 2) viewport'
        let ratio = fromIntegral height / fromIntegral width
        ortho2D (-1) 1 (-ratio) ratio
        matrixMode $= Modelview 0
        draw state
        flush
    return (processHits mhits) -}

-- assumes only one thing is clicked at a time
-- | Converts hitrecords to the thing (a hex, a gui element or nothing)
--   they correspond to.
processHits :: Maybe [HitRecord] -> Maybe HitReturn
processHits (Just (HitRecord _ _ (iden:names):_))
    | iden == hexName = Just (Hex (toPos names))
    | iden == guiName = Just (GUI (toGUIElem names))
  where
    toPos [signY, Name y, signX, Name x] =
        let x' = correctSign signX $ fromIntegral x
            y' = correctSign signY $ fromIntegral y
        in Pos x' y'
    toPos _ = error "Invalid Hex"
    toGUIElem [elem] | elem == selectName = GUISelect
    toGUIElem _ = error "Invalid GUI element"
    correctSign sign = if sign == positiveName then id else negate
processHits _ = Nothing
-}
{-
------------------
-- Mouse motion --
------------------

middlePan :: MVar State -> IO ()
middlePan ref =
    modifyMVar_ ref $ \state -> case state of
        State{mouse = Just MiddleButton, mPos = Just pos} -> do
            (_,size) <- get viewport
            addTimerCallback 10 $ middlePan ref
            postRedisplay Nothing
            return $ state{pan = panWithPos (pan state) pos size, mPos = Just pos}
        _ -> return state

-- | pans the screen a little in the direction indicated by the mouse position.
--   i.e. if the mouse is in the top-left of the screen, pans up and left.
panWithPos :: (Float,Float) -> Position -> Size -> (Float,Float)
panWithPos (panX,panY) (Position x y) (Size w h) = (panX',panY')
    where x' = fromIntegral x
          y' = fromIntegral y
          w' = fromIntegral w
          h' = fromIntegral h
          xMod = (x' * 2) / w' - 1
          yMod = (y' * 2) / h' - 1
          speedMod = 0.7
          panX' = panX - (xMod * speedMod)
          panY' = panY + (yMod * speedMod)
-}