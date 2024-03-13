module Main where

import ExamplesChance (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)

main :: IO ()
main = do
  tileMap <- waveFuncCollapse allTiles ((0, 0, 0), (3, 3, 3))
  putStr $ show tileMap
