{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances #-}

module IO.Draw (draw) where

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT hiding (Object)
import Control.Monad (forM_)

import Game
import IO.Helpers
import IO.State

class Draw a where
    draw :: a -> IO ()

instance Draw State where
    draw (State{pan,zoom,pitch,players,mode}) = do
        preservingMatrix drawGUI
        preservingMatrix $ do
            setView pan zoom
            draw pitch
            draw players
            draw mode

instance Draw Pitch where
    draw pitch = do
        loadName hexName
        forM_ pitch $ \(pos,objects) -> preservingMatrix $ do
            transPos pos
            withPosName pos drawHex 
            translate (Vector3 0 0 0.1 :: Vector3 Float)
            mapM_ draw objects
        where withPosName (Pos (hx,hy)) = withName (coordName hy)
                                        . withName (coordName hx)
              coordName = Name . fromIntegral . toNat

instance Draw Object where
    draw Wall = do
        color blue
        renderPrimitive Polygon hexVerts 
    draw Puck = return ()

instance Draw Players where
    draw = mapM_ $ \(pos,player) -> preservingMatrix (transPos pos >> draw player)

transPos pos = translate (Vector3 x y 0)
    where (x,y) = coord pos

instance Draw Player where
    draw player = return ()

instance Draw Mode where
    draw (Selection (Just pos)) = preservingMatrix $ do
        transPos pos
        translate (Vector3 0 0 0.2 :: Vector3 Float)
        drawHexOutline red 3
    draw _ = return ()

drawHexOutline :: Color4 Float -> Float -> IO ()
drawHexOutline col' lwidth' = do
    lwidth    <- get lineWidth
    lineWidth $= lwidth'
    color col'
    renderPrimitive LineLoop hexVerts
    lineWidth $= lwidth

drawHex :: IO ()
drawHex = preservingMatrix $ do
    color white
    renderPrimitive Polygon hexVerts 
    translate (Vector3 0 0 0.1 :: Vector3 Float)
    color black
    renderPrimitive LineLoop hexVerts

drawPath :: [Pos] -> IO ()
drawPath path = preservingMatrix $ do 
    translate (Vector3 0 0 0.4 :: Vector3 Float)
    color black
    forM_ (pairs $ map coord path) $ \((x0,y0),(x1,y1)) ->
        renderPrimitive Lines $ do
            vertex (Vertex2 x0 y0)
            vertex (Vertex2 x1 y1)

drawGUI :: IO ()
drawGUI = do
    loadName guiName
    drawBox 0    (-1) 0.5 0.2 "Select" selectName

drawBox :: Float -> Float -> Float -> Float -> String -> Name -> IO ()
drawBox x y w h str nm = preservingMatrix $ withName nm $ do
    translate (Vector3 x y 0.9 :: Vector3 Float)
    color black
    preservingMatrix $ do
        let x = 0.0007 :: Float
        translate (Vector3 0.05 0.05 0.1 :: Vector3 Float)
        scale x x x
        renderString MonoRoman str
    color grey
    rect (Vertex2 0 0) (Vertex2 w h)
