-- | This is a generator that generates a heightmap using Perlin noise.
--   The `HeightMap` can be used to generate a world using the "Gen.WaveFuncCollapse" module, or
--   it can be used to generate an image using the "Output" module.
module Gen.PerlinNoise (PerlinConfig(..), perlinNoise, perlinNoiseRandom) where

import Def

import qualified Data.Map as M
import Data.Default
import System.Random
import System.Random.Shuffle (shuffle')
import Data.Bits ((.&.))

-- | The configuration for the Perlin noise generator.
--   The `seed` is the seed used to generate the permutations.
--   The `seed` can also be generated randomly using the `perlinNoiseRandom` function.
data PerlinConfig = PerlinConfig {
    seed :: Int,
    permSize :: Int,
    octaves :: Int,
    scale :: Float,
    persistence :: Float,
    frequency :: Float
}

instance Default PerlinConfig where
    def = PerlinConfig {
        seed = 0,
        permSize = 256,
        octaves = 8,
        scale = 0.1,
        persistence = 0.5,
        frequency = 0.01
    }

-- | The `Permutations` type is a map that maps an integer to another integer.
--   The `Permutations` type is used to generate the noise for the Perlin noise generator.
type Permutations = M.Map Int Int

-- | Dot product of two vectors.
dotProduct :: (Float, Float) -> (Float, Float) -> Float
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- | Get the constant vector for a permutation value.
constantVector :: Int -> (Float, Float)
constantVector permVal 
    | perm == 0 = ( 1,  1)
    | perm == 1 = (-1,  1)
    | perm == 2 = (-1, -1)
    | otherwise = ( 1, -1)
    where perm = permVal .&. 3

-- | Fade function for Perlin noise, this is used to smoothe out the curve of the noise generated.
fade :: Float -> Float
fade t = ((6 * t - 15) * t + 10) * t * t * t

-- | Linear interpolation between three values.
lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)

-- | Shuffle the list of integers to generate a random permutation.
permutationGenerator :: RandomGen g => Int -> g -> Permutations
permutationGenerator size rng = M.fromList $ zip [0 .. size - 1] $ shuffle' [0 .. size - 1] size rng
        
-- | Generate a heightmap using Perlin noise.
perlinNoise :: PerlinConfig -> HeightMap
perlinNoise pcf = 
    let perm = permutationGenerator (permSize pcf) (mkStdGen $ seed pcf)
    in noiseGenerator pcf perm

-- | Generate a heightmap using Perlin noise with a given permutation.
noiseGenerator :: PerlinConfig -> Permutations -> HeightMap
noiseGenerator config perms (x, y) | octaves config == 0 = 0
                                   | otherwise = n / maxValue
    where 
        n :: Float
        maxValue :: Float
        (n, maxValue) = noiseGenerator' config perms 1 (x, y)

-- | Helper function of the `noiseGenerator` function.
--   This function generates the noise for the Perlin noise generator, given the Perlin configuration, the permutations, an amplitude, and a position.
--   The function returns a tuple of the noise value and the maximum value of the noise.
noiseGenerator' :: PerlinConfig -> Permutations -> Float -> (Float, Float) -> (Float, Float)
noiseGenerator' config perms amp (x, y) | octaves config == 0 = (0, 0)
                                        | otherwise = let
    (x', y') = (x * frequency config / scale config, y * frequency config / scale config)
    noise = amp * (noise2D config perms (x', y') + 1) / 2
    (noise', maxValue) = noiseGenerator' (config { octaves = octaves config - 1, frequency = frequency config * 2 }) perms (amp * persistence config) (x, y)
    in (noise + noise', maxValue + amp)

-- | Generate a 2D Perlin noise Height map given its perlin configuration and a (shuffled) `permutations`.
noise2D :: PerlinConfig -> Permutations -> HeightMap
noise2D config perms (x, y) = let 
    (x', y') = (floor x .&. (permSize config - 1), floor y .&. (permSize config - 1))
    (xf, yf) = (x - fromInteger (floor x), y - fromInteger (floor y))
    topRight        = (xf - 1, yf - 1)
    topLeft         = (xf,     yf - 1)
    bottomRight     = (xf - 1, yf    )
    bottomLeft      = (xf,     yf    )
    valueTopRight = perms M.! (((perms M.! ((x' + 1) .&. (permSize config - 1))) + y' + 1) .&. (permSize config - 1))
    valueTopLeft = perms M.! (((perms M.! (x' .&. (permSize config - 1))) + y' + 1) .&. (permSize config - 1))
    valueBottomRight = perms M.! (((perms M.! ((x' + 1) .&. (permSize config - 1))) + y') .&. (permSize config - 1))
    valueBottomLeft = perms M.! (((perms M.! (x' .&. (permSize config - 1))) + y') .&. (permSize config - 1))
    topRightDot     = dotProduct topRight (constantVector valueTopRight)
    topLeftDot      = dotProduct topLeft (constantVector valueTopLeft)
    bottomRightDot  = dotProduct bottomRight (constantVector valueBottomRight)
    bottomLeftDot   = dotProduct bottomLeft (constantVector valueBottomLeft)
    u = fade xf
    v = fade yf
    in lerp u (lerp v bottomLeftDot topLeftDot) (lerp v bottomRightDot topRightDot)

-- | Generate a heightmap using Perlin noise with a random seed.
--   The `seed` value in the `PerlinConfig` is replaced with a random seed.
perlinNoiseRandom :: PerlinConfig -> IO HeightMap
perlinNoiseRandom config = do
    newSeed <- randomIO
    return $ perlinNoise (config { seed = newSeed })
