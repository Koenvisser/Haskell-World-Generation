module Main where

import Data.Default (def)
import qualified Examples.Roads as Roads (allTiles)
import qualified Examples.Mountains as Mountains (airTiles, groundTiles)
import Gen.PerlinNoise (perlinNoiseRandom)
import Gen.WaveFuncCollapse (waveFuncCollapse, waveFuncCollapeHeightMap)
import Output (saveWorldToObjAndMtl)

main :: IO ()
main = generateMountains

generateRoads :: IO ()
generateRoads = do
  result <- waveFuncCollapse Roads.allTiles ((0, 0, 0), (14, 3, 14))
  case result of
    Left err -> putStrLn err
    Right tileMap -> do 
      saveWorldToObjAndMtl tileMap "output" 1
      putStr $ show tileMap

generateMountains :: IO ()
generateMountains = do
  heightMap <- perlinNoiseRandom def
  result <- waveFuncCollapeHeightMap heightMap Mountains.airTiles Mountains.groundTiles ((0, 0, 0), (30, 15, 30))
  case result of
    Left err -> putStrLn err
    Right tileMap -> do 
      saveWorldToObjAndMtl tileMap "output" 1
      putStr $ show tileMap
