module Main where

import Data.List
import Data.Maybe (mapMaybe, catMaybes)
import System.IO

import Ants

gameCrunch :: Wavefront -> [Order]
gameCrunch wavefront
	| isDone wavefront = worder wavefront
	| otherwise        = gameCrunch $ expandWavefront wavefront
	
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  let orders = gameCrunch $ createWavefront (world gs) (ants gs) (food gs)
  return orders

main :: IO ()
main = game doTurn

-- vim: set expandtab:
