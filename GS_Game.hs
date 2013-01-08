module GS_Game
( newState
) where

import Graphics.UI.SDL as SDL hiding (Rect)
import Graphics.Rendering.OpenGL hiding (Rect, get, position, Vector2, Texture)

import Vector2

import Game
import Input
import DrawingCommon

import Texture as Texture

data StateData = StateData { inputHandler :: Input
                           , shouldExit :: Bool
                           , testTexture :: Texture }

initialData input = StateData { inputHandler = input
                              , shouldExit = False
                              , testTexture = newTexture }

instance GameState StateData where
    initialize = GS_Game.initialize
    update = GS_Game.update
    draw = do GS_Game.draw
    isStateFinished = get >>= (\gs -> return (shouldExit gs))

newState :: StateData
newState = initialData newInput

initialize = do
    lift $ texture Texture2D $= Enabled
    loadedTexture <- lift $ Texture.loadTexture "test.png"
    modify (\gs -> gs { testTexture = loadedTexture })
    return ()

update deltaTime = do
    gameState <- get
    handleEvents
    handleInput

handleInput = do
    gameState <- get
    let input = inputHandler gameState
    let altKeyDown = isKeyDown input SDL.SDLK_LALT
                  || isKeyDown input SDL.SDLK_RALT
    if isKeyDown input SDL.SDLK_ESCAPE
      || (altKeyDown && isKeyDown input SDL.SDLK_F4)
        then modify (\gs -> gs { shouldExit = True })
        else return ()

handleEvents = do
    gameState <- get
    let inputObject = inputHandler gameState
    event <- lift SDL.pollEvent
    case event of
        SDL.Quit -> modify (\gs -> gs { shouldExit = True })
        SDL.NoEvent -> return ()
        _ -> do
            modify (\gs -> gs { inputHandler = handleInputEvent event inputObject })
            handleEvents

draw = do
    gameState <- get
    let inputObject = inputHandler gameState
    lift $ do
        clear [ColorBuffer]
        loadIdentity
        bindTexture (testTexture gameState)
        mapM_ (\x -> drawTexture (testTexture gameState) Nothing (Just (Rect x 100 50 50))) [25, 125 .. 1600]
        SDL.glSwapBuffers
