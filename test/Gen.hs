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

instance Arbitrary Side where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Material where
    arbitrary = Material <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Test if all positions are placed in the tilemap if the wave function collapse algorithm is run with a list of tiles 
--   and a size for the world, where the rule for each tile is that it can be placed at any position.
allPosArePlaced :: IO Property
allPosArePlaced = do
    -- Generate a list of tiles that is not empty
    tiles <- generate (listOf1 genTile) :: IO [Tile]
    size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
    -- Execute the wave function collapse algorithm
    (TileMap tileMap) <- waveFuncCollapse tiles size
    -- Generate all positions in the world
    let ((minX, minY, minZ), (maxX, maxY, maxZ)) = size
    let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
    -- Test if all positions are placed
    return $ forAll (elements allPos) $ \pos -> 
        M.member pos tileMap
    where
        -- | Generate a tile, which can be placed at any position
        genTile :: Gen Tile
        genTile = do
            materials <- arbitrary
            rules <- genRule
            charRep <- arbitrary
            return $ Tile materials rules charRep
        -- | Generate a rule, which can be placed at any position
        genRule :: Gen Rule
        genRule = genRuleResult >>= \result -> return $ Rule (\_ _ -> result)
        -- | Generate a rule result, which is always true or has a chance greater than 0 of being placed
        genRuleResult :: Gen RuleResult
        genRuleResult = oneof [return $ CanPlace True, ChancePlace <$> (arbitrary `suchThat` (\f -> f > 0 && f <= 1))]
