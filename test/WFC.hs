{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module WFC where

import WorldGen.Internal.Def
import WorldGen.Def
import WorldGen.Gen.WaveFuncCollapse

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as M

test_WaveFuncCollapse :: [TestTree]
test_WaveFuncCollapse = [
  testProperty "All positions are placed" (ioProperty allPosArePlaced),
  testProperty "All rules are satisfied"  (ioProperty allRulesAreSatisfied),
  testProperty "Size is correct"          (ioProperty sizeIsCorrect)
  ]

-- | Generate a size for the world, given a minimum and maximum size for the world
genSize :: Size -> Gen Size
genSize ((minX, minY, minZ), (maxX, maxY, maxZ)) = do
  minX' <- choose (minX , maxX)
  minY' <- choose (minY , maxY)
  minZ' <- choose (minZ , maxZ)
  maxX' <- choose (minX', maxX)
  maxY' <- choose (minY', maxY)
  maxZ' <- choose (minZ', maxZ)
  return ((minX', minY', minZ'), (maxX', maxY', maxZ'))

instance Arbitrary Side where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Material where
  arbitrary = Material <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Test if all positions are placed in the tilemap if the wave function collapse algorithm is run with a list of tiles 
--   and a size for the world, where the rule for each tile is that it can be placed at any position.
allPosArePlaced :: IO Property
allPosArePlaced = do
  -- Generate a list of tiles that is not empty
  size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
  tiles <- generate $ genTiles size :: IO [Tile]
  -- Execute the wave function collapse algorithm
  result <- waveFuncCollapse tiles size
  case result of
    Left _ -> return $ property False
    Right (TileMap (tileMap, _)) -> do
      -- Generate all positions in the world
      let ((minX, minY, minZ), (maxX, maxY, maxZ)) = size
      let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
      -- Test if all positions are placed
      return $ forAll (elements allPos) $ \pos -> 
        M.member pos tileMap

-- | Test if all rules are satisfied in the tilemap if the wave function collapse algorithm is run with a list of tiles
allRulesAreSatisfied :: IO Property
allRulesAreSatisfied = do
  -- Generate a list of tiles that is not empty
  size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
  tiles <- generate $ genTiles size :: IO [Tile]
  -- Execute the wave function collapse algorithm
  result <- waveFuncCollapse tiles size
  case result of
    Left _ -> return $ property False
    Right (TileMap (tileMap, _)) -> do
      -- Test if all rules are satisfied
      return $ forAll (elements $ M.toList tileMap) $ \(pos, tile) -> 
        let (Rule rule) = rules tile
            (RuleMonad ruleResult _) = rule (TileMap (tileMap, size)) pos
        in resultToBool ruleResult

-- | Test if the size of the tilemap is correct if the wave function collapse algorithm is run with a list of tiles
sizeIsCorrect :: IO Property
sizeIsCorrect = do
  -- Generate a list of tiles that is not empty
  size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
  tiles <- generate $ genTiles size :: IO [Tile]
  -- Execute the wave function collapse algorithm
  result <- waveFuncCollapse tiles size
  case result of
    Left _ -> return $ property False
    Right (TileMap (_,size')) -> do
        -- Test if the size of the tilemap is correct
        return $ size === size'

-- | Generate a list of tiles, which uses the rule from `genRule`
--   For each position in the world, there must be at least one tile that can be placed at that position
genTiles :: Size -> Gen [Tile]
genTiles ((minX, minY, minZ), (maxX, maxY, maxZ)) = listOf1 genTile `suchThat` (\tiles -> all (\pos -> any (\(Tile _ (Rule rule) _) -> 
  let (RuleMonad result _) = rule (TileMap (M.empty, ((0,0,0), (0,0,0)))) pos in resultToBool result) tiles) positions)
  where positions = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]

-- | Generate a tile, which uses the rule from `genRule`
genTile :: Gen Tile
genTile = Tile <$> arbitrary <*> genRule <*> arbitrary

-- | Generate a rule, which ignores the tileMap for performance reasons
genRule :: Gen Rule
genRule = arbitrary >>= \f -> return $ Rule $ const f

instance Arbitrary RuleResult where
  arbitrary = oneof [CanPlace <$> arbitrary, ChancePlace <$> (arbitrary `suchThat` (\f -> f >= 0 && f <= 1))]

instance Arbitrary (RuleMonad RuleResult) where
  arbitrary = RuleMonad <$> arbitrary <*> arbitrary
