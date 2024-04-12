module Main where

import Data.Default (def)
import qualified Examples.Roads as Roads (allTiles)
import qualified Examples.Mountains as Mountains (airTiles, groundTiles)
import Gen.PerlinNoise (perlinNoiseRandom)
import Gen.WaveFuncCollapse (waveFuncCollapse, waveFuncCollapeHeightMap)
import Output (saveWorldToObjAndMtl, saveHeightMapToImage)

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
  saveHeightMapToImage heightMap 1000 1000 "output/image.png"
  result <- waveFuncCollapeHeightMap heightMap Mountains.airTiles Mountains.groundTiles ((0, 0, 0), (10, 8, 10))
  case result of
    Left err -> putStrLn err
    Right tileMap -> do 
      saveWorldToObjAndMtl tileMap "output" 1
      putStr $ show tileMap
