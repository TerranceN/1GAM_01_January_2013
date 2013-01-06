module Game
( Game
, runGameState
, GameState(..)
, module Control.Monad.Trans
, module Control.Monad.Trans.State.Lazy
) where

import Graphics.UI.SDL as SDL hiding (update)
import Graphics.Rendering.OpenGL hiding (Rect, get, position, Vector2)

import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

type GameStateMonad a b = StateT a IO b

class GameState a where
    initialize :: GameStateMonad a ()
    initialize = return ()

    update :: Double -> GameStateMonad a ()
    update deltaTime = return ()

    draw :: GameStateMonad a ()
    draw = return ()

    isStateFinished :: GameStateMonad a Bool
    isStateFinished = return True

type Game s x = StateT s IO x

runGameState :: GameState a => Int -> a -> IO ()
runGameState framerate state = runStateT (initialize >> gameLoop 0) state >> return ()
  where
    maxDelay = 1000 `quot` framerate
    gameLoop :: GameState a => Int -> GameStateMonad a ()
    gameLoop lastTime = do
        let delayAmount = maxDelay - lastTime
        if delayAmount > 0
            then lift $ SDL.delay (fromIntegral delayAmount)
            else return ()
        beginTime <- lift SDL.getTicks
        update ((fromIntegral (lastTime + delayAmount)) / (fromIntegral maxDelay))
        draw
        isDone <- isStateFinished
        endTime <- lift SDL.getTicks
        if isDone
            then return ()
            else gameLoop (fromIntegral (endTime - beginTime))
