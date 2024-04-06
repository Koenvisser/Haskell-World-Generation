module Main where

import Examples.Roads (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)
import Output (saveWorldToObjAndMtl)

main :: IO ()
main = do
  result <- waveFuncCollapse allTiles ((0, 0, 0), (29, 3, 29))
  case result of
    Left err -> putStrLn err
    Right tileMap -> do 
      saveWorldToObjAndMtl tileMap "output" 1
      putStr $ show tileMap
