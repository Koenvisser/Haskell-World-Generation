module Gen where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as M

import Def
import Gen.WaveFuncCollapse

test_WaveFuncCollapse :: [TestTree]
test_WaveFuncCollapse = [
    testProperty "All positions are placed" (ioProperty allPosArePlaced)
    ]

instance Arbitrary Tile where
    arbitrary = do
        textureLoc <- arbitrary
        rules <- arbitrary >>= \result -> return $ Rule (\_ _ -> result) 
        charRep <- arbitrary
        return $ Tile textureLoc rules charRep

instance Arbitrary Rule where
    arbitrary = arbitrary >>= \result -> return $ Rule (\_ _ -> result)

instance Arbitrary TileMap where
    arbitrary = do
        tileMap <- arbitrary
        return $ TileMap tileMap

instance Arbitrary RuleResult where
    arbitrary = do
        b <- arbitrary :: Gen Bool
        f <- arbitrary `suchThat` (\f -> f >= 0 && f <= 1) :: Gen Float
        elements [CanPlace b, ChancePlace f]

instance CoArbitrary RuleResult where
    coarbitrary (CanPlace b) = coarbitrary b
    coarbitrary (ChancePlace f) = coarbitrary f

instance CoArbitrary TileMap where
    coarbitrary (TileMap tileMap) = coarbitrary tileMap

instance CoArbitrary Tile where
    coarbitrary (Tile textureLoc rules charRep) = coarbitrary (textureLoc, rules, charRep)

instance CoArbitrary Rule where
    coarbitrary (Rule rule) = coarbitrary rule

-- | Generate a size for the world, given a minimum and maximum size for the world
genSize :: Size -> Gen Size
genSize ((minX, minY, minZ), (maxX, maxY, maxZ)) = do
    minX' <- choose (minX, maxX)
    minY' <- choose (minY, maxY)
    minZ' <- choose (minZ, maxZ)
    maxX' <- choose (minX', maxX)
    maxY' <- choose (minY', maxY)
    maxZ' <- choose (minZ', maxZ)
    return ((minX', minY', minZ'), (maxX', maxY', maxZ'))

-- | Test if all positions are placed in the tilemap if the wave function collapse algorithm is run with a list of tiles 
--   and a size for the world, where the rule for each tile is that it can be placed at any position.
allPosArePlaced :: IO Property
allPosArePlaced = do
    tiles <- generate (listOf arbitrary) :: IO [Tile]
    size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
    (TileMap tileMap) <- waveFuncCollapse tiles size
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = size
    let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
    return $ forAll (elements allPos) $ \pos -> 
        M.member pos tileMap

