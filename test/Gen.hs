module Gen where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as M

import Def
import Gen.WaveFuncCollapse

test_WaveFuncCollapse :: [TestTree]
test_WaveFuncCollapse = [
    testProperty "All positions are placed" (ioProperty prop_allPosArePlaced)
    ]

instance Arbitrary Tile where
    arbitrary = do
        textureLoc <- arbitrary
        rules <- arbitrary
        charRep <- arbitrary
        return $ Tile textureLoc rules charRep

instance Arbitrary Rule where
    arbitrary = do
        rule <- arbitrary
        return $ Rule rule

instance Arbitrary TileMap where
    arbitrary = do
        tileMap <- arbitrary
        return $ TileMap tileMap

instance Arbitrary RuleResult where
    arbitrary = do
        result <- arbitrary
        return $ result

instance CoArbitrary RuleResult where
    coarbitrary (CanPlace b) = coarbitrary b
    coarbitrary (ChancePlace f) = coarbitrary f

instance CoArbitrary TileMap where
    coarbitrary (TileMap tileMap) = coarbitrary tileMap

instance CoArbitrary Tile where
    coarbitrary tile = coarbitrary (textureLoc tile, rules tile, charRep tile)

instance CoArbitrary Rule where
    coarbitrary rule = coarbitrary (case rule of Rule f -> f)

prop_allPosArePlaced :: IO Property
prop_allPosArePlaced = do
    tiles <- generate arbitrary :: IO [Tile]
    size <- generate arbitrary :: IO Size
    (TileMap tileMap) <- waveFuncCollapse tiles size
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = size
    let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
    return $ forAll (elements allPos) $ \pos -> 
        M.member pos tileMap
