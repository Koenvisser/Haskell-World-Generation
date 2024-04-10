module Gen.PerlinNoise where

import Def

import qualified Data.Map as M
import Data.Default
import System.Random

type Size2D = (Int, Int)
type Seed = Int
type Octaves = Int
type Scale = Float
type Persistance = Float

data PerlinConfig = PerlinConfig {
    seed :: Seed,
    octaves :: Octaves,
    scale :: Scale,
    persistance :: Persistance
}

instance Default PerlinConfig where
    def = PerlinConfig {
        seed = 0,
        octaves = 8,
        scale = 0.1,
        persistance = 0.5
    }

-- | Generate a heightmap using Perlin noise.
perlinNoise :: Size2D -> PerlinConfig -> HeightMap
perlinNoise = undefined

perlinNoiseRandom :: Size2D -> PerlinConfig -> IO HeightMap
perlinNoiseRandom size config = do
    seed <- randomIO
    return $ perlinNoise size config { seed = seed }


