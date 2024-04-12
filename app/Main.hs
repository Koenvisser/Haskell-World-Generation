module Main where

import qualified WorldGen.Examples.Roads as Roads (allTiles)
import qualified WorldGen.Examples.Mountains as Mountains (airTiles, groundTiles)
import WorldGen.Gen.PerlinNoise (perlinNoiseRandom)
import WorldGen.Gen.WaveFuncCollapse (waveFuncCollapse, waveFuncCollapeHeightMap)
import WorldGen.Output (saveWorldToObjAndMtl, saveHeightMapToImage)

import Data.Default (def)

main :: IO ()
main = generateRoads

-- | Generate an example world with roads
generateRoads :: IO ()
generateRoads = do
  tiles <- Roads.allTiles
  result <- waveFuncCollapse tiles ((0, 0, 0), (5, 3, 5))
  case result of
    Left err -> putStrLn err
    Right tileMap -> do 
      saveWorldToObjAndMtl tileMap "output" 1
      putStr $ show tileMap

-- | Generate an example world with mountains
generateMountains :: IO ()
generateMountains = do
  heightMap <- perlinNoiseRandom def
  saveHeightMapToImage heightMap 1000 1000 "output/image.png"
  air <- Mountains.airTiles
  ground <- Mountains.groundTiles
  result <- waveFuncCollapeHeightMap heightMap air ground ((0, 0, 0), (19, 9, 19))
  case result of
    Left err -> putStrLn err
    Right tileMap -> do 
      saveWorldToObjAndMtl tileMap "output" 1
      putStr $ show tileMap
