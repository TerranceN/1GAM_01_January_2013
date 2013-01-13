module Map
( Map()
, newMap
, addTile
, addTiles
, drawMap
) where

import Data.List

import Graphics.Rendering.OpenGL hiding (Rect, get, position, Vector2, Texture)
import DrawingCommon

import Texture

type TexLocations = (Texture, [Position])
type Map = [TexLocations]

tileSize = 50

newMap :: Map
newMap = []

addTile :: Texture -> Position -> Map -> Map
addTile tex pos [] = [(tex, [pos])]
addTile tex pos map@(texLoc@(xTex, xPositions):xs)
    | tex == xTex =
        if pos `elem` xPositions
            then map
            else ((xTex, pos:xPositions)):xs
    | otherwise = texLoc:(addTile tex pos xs)

addTiles :: Texture -> [Position] -> Map -> Map
addTiles tex pos [] = [(tex, pos)]
addTiles tex pos map@(texLoc@(xTex, xPositions):xs)
    | tex == xTex = ((xTex, pos `union` xPositions)):xs
    | otherwise = texLoc:(addTiles tex pos xs)

drawMap :: Map -> IO ()
drawMap [] = return ()
drawMap ((tex, posns):xs) = do
    bindTexture tex
    mapM_ (drawTile) posns
    drawMap xs
  where
    drawTile (Position x y) = do
        let dstRect = Rect ((fromIntegral x) * tileSize)
                           ((fromIntegral y) * tileSize)
                           tileSize tileSize
        drawTexture tex Nothing (Just dstRect)
