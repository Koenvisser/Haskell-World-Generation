{-# OPTIONS_GHC -Wno-orphans #-}
module Perlin where

import WorldGen.Gen.PerlinNoise (PerlinConfig(..), perlinNoiseRandom)

import Test.Tasty
import Test.Tasty.QuickCheck

test_Perlin :: [TestTree]
test_Perlin = [
  testProperty "Height is between 0 and 1" (ioProperty heightIsBetweenZeroAndOne)
  ]

-- | Test if the height of the Perlin noise generator is between 0 and 1.
heightIsBetweenZeroAndOne :: IO Property
heightIsBetweenZeroAndOne = do
  -- Generate a Perlin configuration
  config <- generate arbitrary :: IO PerlinConfig
  -- Execute the Perlin noise generator
  heightMap <- perlinNoiseRandom config
  -- Generate two random floats
  x <- generate arbitrary :: IO Float
  y <- generate arbitrary :: IO Float
  -- Test if the height is between 0 and 1
  let height = heightMap (x, y)
  return $ property (height >= 0 && height <= 1)

instance Arbitrary PerlinConfig where
  arbitrary = PerlinConfig <$> arbitrary 
    <*> (arbitrary `suchThat` (\size -> size >= 1 && size <= 10)) 
    <*> (arbitrary `suchThat` (\oct -> oct >= 0 && oct <= 10)) 
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
