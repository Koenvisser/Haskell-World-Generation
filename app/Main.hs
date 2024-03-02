module Main where

import Examples
import Gen (generate)

main :: IO ()
main = do
  let tileMap = generate allTiles
  putStr $ show tileMap

