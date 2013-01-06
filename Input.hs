module Input
( Input()
, newInput
, handleInputEvent
, isKeyDown
, getMousePosition
, getKeysDown
) where

import Data.IORef

import Graphics.UI.SDL as SDL

data Input = Input { keysDown :: [SDL.SDLKey]
                   , keysDownOnce :: [SDL.SDLKey]
                   , mouseX :: Int
                   , mouseY :: Int
                   } deriving (Show)

newInput :: Input
newInput = Input { keysDown = []
                 , keysDownOnce = []
                 , mouseX = 0
                 , mouseY = 0
                 }

handleInputEvent :: SDL.Event -> Input -> Input
handleInputEvent (KeyDown k) = addKey (symKey k)
handleInputEvent (KeyUp k) = removeKey (symKey k)
handleInputEvent (MouseMotion x y dx dy) =
    setPosition (fromIntegral x) (fromIntegral y)
handleInputEvent _ = (\x -> x)

addKey :: SDLKey -> Input -> Input
addKey key input =
    input { keysDown = key:(keysDown input) }

removeKey :: SDLKey -> Input -> Input
removeKey key input =
    input { keysDown = filter (/= key) (keysDown input) }

setPosition :: Int -> Int -> Input -> Input
setPosition x y input =
    input { mouseX = x, mouseY = y }

isKeyDown :: Input -> SDLKey -> Bool
isKeyDown input key = elem key (keysDown input)

getMousePosition :: Input -> (Int, Int)
getMousePosition input = (mouseX input, mouseY input)

getKeysDown :: Input -> [SDL.SDLKey]
getKeysDown = keysDown
