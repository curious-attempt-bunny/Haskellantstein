module Main where

import Data.List
import Data.Maybe (mapMaybe, catMaybes)
import System.IO

import Ants

tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

ordersForFood :: GameState -> [Order]
ordersForFood gs = orderForFood (world gs) (food gs) (ants gs)
	
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
--  let generatedOrders = map generateOrders $ myAnts $ ants gs
--      orders = mapMaybe (tryOrder (world gs)) generatedOrders
  let orders = ordersForFood gs
  return orders

main :: IO ()
main = game doTurn

-- vim: set expandtab:
