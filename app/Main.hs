module Main where

import RoadExample (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)
import Output (saveWorldToObjAndMtl)

main :: IO ()
main = do
  tileMap <- waveFuncCollapse allTiles ((0, 0, 0), (14, 3, 14))
  saveWorldToObjAndMtl tileMap "output" 1 True
  putStr $ show tileMap
