module Main where

import Examples (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)

main :: IO ()
main = do
  tileMap <- waveFuncCollapse allTiles ((0, 0, 0), (5, 5, 5))
  putStr $ show tileMap
