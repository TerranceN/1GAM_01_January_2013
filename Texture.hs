module Texture
( Texture()
, Texture.loadTexture
, newTexture
, bindTexture
, drawTexture
, Rect(..)
) where

import Graphics.Rendering.OpenGL hiding (Rect, get, position, Vector2, Texture)

import Data.Vector.Storable hiding (modify)
import Data.Word

import Graphics.GLUtil as GLUtil
import Codec.Picture as Pic

import DrawingCommon

data Rect a = Rect a a a a

data Texture = Texture { getTexObj :: Maybe TextureObject
                       , getTexWidth :: Int
                       , getTexHeight :: Int
                       , getTexColor :: TexColor }

newTexture = Texture { getTexObj = Nothing
                     , getTexWidth = 0
                     , getTexHeight = 0
                     , getTexColor = TexRGB }

loadTexture :: FilePath -> IO Texture
loadTexture path = do
    maybeImage <- readImage path
    case maybeImage of  
        Left str -> return newTexture
        Right img -> dynImgToTex img

getImageData :: DynamicImage -> (TexColor, (Int, Int, Vector Word8))
getImageData dImg =
    case dImg of
        ImageY8 img -> (TexMono, imgData img)
        ImageYA8 img -> (TexMono, imgData img)
        ImageRGB8 img -> (TexRGB, imgData img)
        ImageRGBA8 img -> (TexRGBA, imgData img)
        ImageYCbCr8 img -> (TexBGR, imgData img)
  where
    imgData img = (imageWidth img, Pic.imageHeight img, imageData img)

dynImgToTex :: DynamicImage -> IO Texture
dynImgToTex img = do
    let (imgType, (w, h, iData)) = getImageData img
    loadedTexture <- GLUtil.loadTexture (texInfo w h imgType (toList iData))
    textureFilter   Texture2D   $= ((Linear', Nothing), Nearest)
    textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
    textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
    return $ Texture { getTexObj = Just loadedTexture
                     , getTexWidth = w
                     , getTexHeight = h
                     , getTexColor = imgType }

getTexWidthf :: Texture -> Double
getTexWidthf tex = (fromIntegral (getTexWidth tex)) :: Double

getTexHeightf :: Texture -> Double
getTexHeightf tex = (fromIntegral (getTexHeight tex)) :: Double

bindTexture :: Texture -> IO ()
bindTexture tex = do
    texture Texture2D $= Enabled
    textureBinding Texture2D $= (getTexObj tex)

drawTexture :: Texture -> Maybe (Rect Double) -> Maybe (Rect Double) -> IO ()
drawTexture tex Nothing (Just dst) =
    drawTexture tex (Just (Rect 0 0 (getTexWidthf tex) (getTexHeightf tex))) (Just dst)
drawTexture tex (Just src) Nothing =
    drawTexture tex (Just src) (Just (Rect 0 0 (getTexWidthf tex) (getTexHeightf tex)))
drawTexture tex (Just (Rect x1 y1 w1 h1)) (Just (Rect x2 y2 w2 h2)) = do
    let xFraction = x1 / (getTexWidthf tex)
    let yFraction = y1 / (getTexHeightf tex)
    let widthFraction = w1 / (getTexWidthf tex)
    let heightFraction = h1 / (getTexWidthf tex)
    renderPrimitive Quads $ do
        texCoord $ toTexCoord2f xFraction yFraction
        vertex $ toVertex2f x2 y2
        texCoord $ toTexCoord2f (xFraction + widthFraction) yFraction
        vertex $ toVertex2f (x2 + w2) y2
        texCoord $ toTexCoord2f (xFraction + widthFraction) (yFraction + heightFraction)
        vertex $ toVertex2f (x2 + w2) (y2 + h2)
        texCoord $ toTexCoord2f xFraction (yFraction + heightFraction)
        vertex $ toVertex2f x2 (y2 + h2)

