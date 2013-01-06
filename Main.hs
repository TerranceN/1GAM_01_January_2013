module Main
( main
) where

import Game

import qualified GS_Init
import qualified GS_Game

main = Game.runGameState 60 (GS_Init.newState GS_Game.newState)
