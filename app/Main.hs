module Main where

import Def
import RoadExample (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)
import Output (saveWorldToObjAndMtl)

main :: IO ()
main = do
  tileMap <- waveFuncCollapse allTiles ((0, 0, 0), (14, 3, 14))
  let world = World (((0, 0, 0), (14, 3, 14)), tileMap)
  saveWorldToObjAndMtl world "output" 1 True
  putStr $ show tileMap
