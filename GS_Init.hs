module GS_Init
( newState
) where

import Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL hiding (Rect, get, position, Vector2)

import Game
import Input

import PutEnv

data StateData = StateData { windowSize :: (Int, Int)
                           , nextState :: IO () }

initialData gameState = StateData { windowSize = (1600, 900)
                                  , nextState = runGameState 60 gameState }

instance GameState StateData where
    initialize = do
        gameState <- get
        GS_Init.init
        lift $ nextState gameState
        return ()

newState gameState = initialData gameState

initOpenGL = do
    gameState <- get
    let convert = (\(x, y) -> (fromIntegral x, fromIntegral y))
    let (windowWidth, windowHeight) = convert $ windowSize gameState
    lift $ do
        clearColor $= Color4 (0 :: GLclampf) 0 0 0
        matrixMode $= Projection
        loadIdentity    
        ortho 0 windowWidth windowHeight 0 (-1) 1
        matrixMode $= Modelview 0
        loadIdentity

init = do
    gameState <- get
    let (windowWidth, windowHeight) = windowSize gameState
    lift $ do
        putEnv "SDL_VIDEO_CENTERED=center"
        SDL.init [SDL.InitEverything]
        SDL.setVideoMode windowWidth windowHeight 32 [SDL.OpenGL, SDL.NoFrame]
        SDL.setCaption "Video Test!!!" "video test"
        SDL.grabInput True
    initOpenGL
