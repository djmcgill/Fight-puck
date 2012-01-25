module Main where

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import Control.Concurrent.MVar

import IO.State (State, initialState)
import IO.Draw (draw)
import IO.KBM (kbm, motion)

main = do 
    initialDisplayMode    $= [RGBAMode, DoubleBuffered, WithAlphaComponent]
    initialWindowSize     $= Size 600 600
    getArgsAndInitialize
    createWindow "Fight Puck"
    stateRef              <- newMVar initialState
    displayCallback       $= (render stateRef)
    keyboardMouseCallback $= Just (kbm stateRef)
    motionCallback        $= Just (motion stateRef)
    depthFunc  $= Just Less
    clearColor $= (Color4 1 1 1 1)
    mainLoop

render :: MVar State -> IO ()
render ref = do
    clear [DepthBuffer, ColorBuffer]
    matrixMode $= Projection
    loadIdentity
    (_,Size width height) <- get viewport
    let ratio = fromIntegral height / fromIntegral width
    ortho2D (-1) 1 (-ratio) ratio
    matrixMode $= Modelview 0
    loadIdentity

    color (Color4 0 0 0 1 :: Color4 Float)
    state <- readMVar ref
    draw state

    flush
    swapBuffers
