module Camera
( Camera()
) where

import Vector2

data Camera = Camera { translation :: Vector2
                     , rotation :: Double
                     , scale :: Double
                     }

newCamera :: Camera
newCamera = Camera { translation = (0, 0)
                   , rotation = 0.0
                   , scale = 0.0

cameraForceFocus :: Vector2 -> Camera -> Camera
cameraForceFocus vec cam = cam { translation = 
