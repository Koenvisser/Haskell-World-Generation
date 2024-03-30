module Main where

import Examples (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)

main :: IO ()
main = do
  tileMap <- waveFuncCollapse allTiles ((0, 0, 0), (0, 1, 1))
  putStr $ show tileMap
