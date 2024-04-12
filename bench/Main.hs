module Main where

import Examples.Roads (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)
import Gen.PerlinNoise (perlinNoiseRandom)
import Criterion.Main
import System.Random (randomIO)
import Data.Default (def)

main :: IO ()
main = defaultMain [
  bgroup "waveFuncCollapse" [bench "waveFuncCollapse" $ nfIO $ waveFuncCollapse allTiles ((0, 0, 0), (9, 3, 9))],
  bgroup "perlinNoiseRandom" [bench "perlinNoiseRandom" $ nfIO $ do 
    x <- randomIO :: IO Float
    y <- randomIO :: IO Float
    func <- perlinNoiseRandom def
    return $ func (x, y)]
  ]
