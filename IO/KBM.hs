module IO.KBM (kbm, motion) where

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import System.Exit (exitSuccess)
import Control.Concurrent.MVar

import Game (Pos(..))
import IO.State
import IO.Helpers
import IO.Draw

-----------------
-- Mouse click --
-----------------

kbm :: MVar State -> KeyboardMouseCallback
kbm _   (Char '\27') Down _ _ = exitSuccess
kbm ref (Char c)     Down _ _ = do 
    modifyMVar_ ref $ \state -> case c of 
        -- reset all the variables
        '.' -> return initialState
        -- iterate all the things (not yet implemented)
        ',' -> return $ updateState state
        -- zoom
        '[' -> return $ state {zoom = zoom state * (9/10)}
        ']' -> return $ state {zoom = zoom state * (10/9)}
        -- pan
        'w' -> return $ state {pan = panUp    (pan state)}
        's' -> return $ state {pan = panDown  (pan state)}
        'a' -> return $ state {pan = panLeft  (pan state)}
        'd' -> return $ state {pan = panRight (pan state)}
        _   -> return state
    postRedisplay Nothing
kbm ref (MouseButton LeftButton) Down _ mouse = do
-- change which hex is selected
    modifyMVar_ ref $ \state' -> do
        let state = state'{mouse = Just LeftButton}
        hit <- getHit state mouse
        return $ case hit of
            Just (Hex hex) | isSelection state -> setSelection hex state
            Just (GUI GUISelect) -> state {mode = Selection Nothing}
            _ -> state
    postRedisplay Nothing
kbm ref (MouseButton RightButton) Down _ mouse = do
-- move the selected hex
    modifyMVar_ ref $ \state' ->
        return $ state'{mouse = Just RightButton}
kbm ref (MouseButton MiddleButton) Down _ mouse = do
    modifyMVar_ ref $ \state -> 
        return $ state {mouse = Just MiddleButton, mPos = Just mouse}
    addTimerCallback 10 $ middlePan ref
kbm ref (MouseButton _) Up _ _ =
    mouseUp ref
kbm _ _ _ _ _ = return ()

panUp    (x,y) = (x  ,y-1)
panDown  (x,y) = (x  ,y+1)
panLeft  (x,y) = (x+1,y  )
panRight (x,y) = (x-1,y  )

getHit :: State -> Position -> IO (Maybe HitReturn)
getHit state (Position x y) = do
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
    return (processHits mhits)

-- assumes only one thing is clicked at a time
-- | Converts hitrecords to the thing (a hex, a gui element or nothing)
--   they correspond to.
processHits :: Maybe [HitRecord] -> Maybe HitReturn
processHits (Just (HitRecord _ _ (iden:names):_))
    | iden == hexName = Just (Hex (toPos names))
    | iden == guiName = Just (GUI (toGUIElem names))
  where
    toPos [Name y,Name x] =
        let x' = toInt . fromIntegral $ x
            y' = toInt . fromIntegral $ y
        in Pos (x',y')
    toPos _ = error "Invalid Hex"
    toGUIElem [elem] | elem == selectName = GUISelect
    toGUIElem _ = error "Invalid GUI element"
processHits _ = Nothing

mouseUp :: MVar State -> IO ()
mouseUp ref = modifyMVar_ ref $ \state ->
    return $ state{mouse = Nothing, mPos = Nothing}

------------------
-- Mouse motion --
------------------
motion :: MVar State -> Position -> IO ()
motion ref pos = do
    modifyMVar_ ref $ \state -> case mPos state of
        Just _  -> return $ state{mPos = Just pos}
        Nothing -> return state

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
