module Main where

import Examples.Roads (allTiles)
import Gen.WaveFuncCollapse (waveFuncCollapse)
import Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "waveFuncCollapse" [bench "waveFuncCollapse" $ nfIO $ waveFuncCollapse allTiles ((0, 0, 0), (14, 3, 14))]
  ]
