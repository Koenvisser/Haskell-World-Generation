module Main where

import Examples.Roads (allTiles)
import Examples.Mountains (airTiles, groundTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse, waveFuncCollapeHeightMap)
import Gen.PerlinNoise (perlinNoiseRandom)
import Criterion.Main
import System.Random (randomIO)
import Data.Default (def)

main :: IO ()
main = defaultMain [
  bgroup "PerlinNoseCollapse" [bench "PerlinNoseCollapse" $ nfIO $ do 
    heightMap <- perlinNoiseRandom def
    waveFuncCollapeHeightMap heightMap airTiles groundTiles ((0, 0, 0), (15, 9, 15))],
  bgroup "waveFuncCollapse" [bench "waveFuncCollapse" $ nfIO $ waveFuncCollapse allTiles ((0, 0, 0), (15, 9, 15))],
  bgroup "perlinNoiseRandom" [bench "perlinNoiseRandom" $ nfIO $ do 
    x <- randomIO :: IO Float
    y <- randomIO :: IO Float
    func <- perlinNoiseRandom def
    return $ func (x, y)]
  ]
