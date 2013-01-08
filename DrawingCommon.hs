module DrawingCommon where

import Vector2
import Graphics.UI.SDL as SDL hiding (update)
import Graphics.Rendering.OpenGL hiding (Rect, get, position, Vector2)

drawQuadAtPoint :: Vector2 -> Double -> IO ()
drawQuadAtPoint (x, y) size = do
    renderPrimitive Quads $ do
        vertex $ toVertex2f (x - size) (y - size)
        vertex $ toVertex2f (x + size) (y - size)
        vertex $ toVertex2f (x + size) (y + size)
        vertex $ toVertex2f (x - size) (y + size)

toVertex2 :: (Integral a) => a -> a -> Vertex2 GLfloat
toVertex2 a b = Vertex2 ((fromIntegral a) :: GLfloat) ((fromIntegral b) :: GLfloat)

toVertex2f :: (Real a) => a -> a -> Vertex2 GLfloat
toVertex2f a b = Vertex2 ((convertf a) :: GLfloat) ((convertf b) :: GLfloat)

toTexCoord2f :: (Real a) => a -> a -> TexCoord2 GLfloat
toTexCoord2f a b = TexCoord2 ((convertf a) :: GLfloat) ((convertf b) :: GLfloat)

convertf :: Real a => Fractional b => a -> b
convertf = fromRational . toRational
