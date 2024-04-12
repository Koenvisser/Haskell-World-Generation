module Main where

import WorldGen.Examples.Roads (allTiles)
import WorldGen.Examples.Mountains (airTiles, groundTiles)
import WorldGen.Gen.WaveFuncCollapse (waveFuncCollapse, waveFuncCollapeHeightMap)
import WorldGen.Gen.PerlinNoise (perlinNoiseRandom)

import Criterion.Main
import System.Random (randomIO)
import Data.Default (def)

main :: IO ()
main = defaultMain [
  bgroup "PerlinNoseCollapse" [bench "PerlinNoseCollapse" $ nfIO $ do 
    heightMap <- perlinNoiseRandom def
    air <- airTiles
    ground <- groundTiles
    waveFuncCollapeHeightMap heightMap air ground ((0, 0, 0), (9, 9, 9))],
  bgroup "waveFuncCollapse" [bench "waveFuncCollapse" $ nfIO $ waveFuncCollapse allTiles ((0, 0, 0), (9, 9, 9))],
  bgroup "perlinNoiseRandom" [bench "perlinNoiseRandom" $ nfIO $ do 
    x <- randomIO :: IO Float
    y <- randomIO :: IO Float
    func <- perlinNoiseRandom def
    return $ func (x, y)]
  ]
