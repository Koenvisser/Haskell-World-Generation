module Gen.PerlinNoise where

import qualified Data.Map as M
import Data.Default
import System.Random
import System.Random.Shuffle (shuffle')

type Size2D = ((Int, Int), (Int, Int))
type Seed = Int
type Octaves = Int
type Scale = Float
type Persistance = Float

data PerlinConfig = PerlinConfig {
    seed :: Seed,
    permSize :: Int,
    octaves :: Octaves,
    scale :: Scale,
    persistance :: Persistance,
    frequency :: Float
}

instance Default PerlinConfig where
    def = PerlinConfig {
        seed = 0,
        permSize = 256,
        octaves = 8,
        scale = 1,
        persistance = 0.5,
        frequency = 0.01
    }

-- | A heightmap is a map from (x, y) coordinates to a height value.
type HeightMap = (Float, Float) -> Float

type Permutations = M.Map Int Int

dotProduct :: (Float, Float) -> (Float, Float) -> Float
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

constantVector :: Int -> (Float, Float)
constantVector permVal 
    | perm == 0 = ( 1,  1)
    | perm == 1 = (-1,  1)
    | perm == 2 = (-1, -1)
    | otherwise = ( 1, -1)
    where perm = permVal `mod` 3

fade :: Float -> Float
fade t = ((6 * t - 15) * t + 10) * t * t * t

lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)

permutationGenerator :: RandomGen g => Int -> g -> Permutations
permutationGenerator size rng = M.fromList $ zip [0 .. size - 1] $ shuffle' [0 .. size - 1] size rng
        
-- | Generate a heightmap using Perlin noise.
perlinNoise :: PerlinConfig -> HeightMap
perlinNoise pcf = 
    let perm = permutationGenerator (permSize pcf) (mkStdGen $ seed pcf)
    in noiseGenerator pcf perm

noiseGenerator :: PerlinConfig -> Permutations -> HeightMap
noiseGenerator config perms (rawX,rawY) = let 
    (x, y) = (rawX * frequency config, rawY * frequency config)
    (x', y') = (floor x `mod` (permSize config - 1), floor y `mod` (permSize config - 1))
    (xf, yf) = (x - fromIntegral (floor x), y - fromIntegral (floor y))
    topRight        = (xf - 1, yf - 1)
    topLeft         = (xf,     yf - 1)
    bottomRight     = (xf - 1, yf    )
    bottomLeft      = (xf,     yf    )
    valueTopRight   = perms M.! (((perms M.! (x' + 1)) + y' + 1) `mod` (permSize config - 1))
    valueTopLeft    = perms M.! (((perms M.! x') + y' + 1) `mod` (permSize config - 1))
    valueBottomRight= perms M.! (((perms M.! (x' + 1)) + y') `mod` (permSize config - 1))
    valueBottomLeft = perms M.! (((perms M.! x') + y') `mod` (permSize config - 1))
    topRightDot     = dotProduct topRight (constantVector valueTopRight)
    topLeftDot      = dotProduct topLeft (constantVector valueTopLeft)
    bottomRightDot  = dotProduct bottomRight (constantVector valueBottomRight)
    bottomLeftDot   = dotProduct bottomLeft (constantVector valueBottomLeft)
    u = fade xf
    v = fade yf
    in lerp u (lerp v bottomLeftDot topLeftDot) (lerp v bottomRightDot topRightDot)

perlinNoiseRandom :: PerlinConfig -> IO HeightMap
perlinNoiseRandom config = do
    seed <- randomIO
    return $ perlinNoise (config { seed = seed })
