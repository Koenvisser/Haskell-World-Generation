module Gen where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map as M

import Def
import Gen.WaveFuncCollapse

test_WaveFuncCollapse :: [TestTree]
test_WaveFuncCollapse = [
    testProperty "All positions are placed" (ioProperty allPosArePlaced),
    testProperty "All rules are satisfied" (ioProperty allRulesAreSatisfied)
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
    arbitrary = Material <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Test if all positions are placed in the tilemap if the wave function collapse algorithm is run with a list of tiles 
--   and a size for the world, where the rule for each tile is that it can be placed at any position.
allPosArePlaced :: IO Property
allPosArePlaced = do
    -- Generate a list of tiles that is not empty
    tiles <- generate genTiles :: IO [Tile]
    size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
    -- Execute the wave function collapse algorithm
    result <- waveFuncCollapse tiles size
    case result of
        Left err -> return $ property False
        Right (TileMap tileMap) -> do
            -- Generate all positions in the world
            let ((minX, minY, minZ), (maxX, maxY, maxZ)) = size
            let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
            -- Test if all positions are placed
            return $ forAll (elements allPos) $ \pos -> 
                M.member pos tileMap

allRulesAreSatisfied :: IO Property
allRulesAreSatisfied = do
    -- Generate a list of tiles that is not empty
    tiles <- generate genTiles :: IO [Tile]
    size <- generate (genSize ((0, 0, 0), (2, 2, 2))) :: IO Size
    -- Execute the wave function collapse algorithm
    result <- waveFuncCollapse tiles size
    case result of
        Left err -> return $ property False
        Right (TileMap tileMap) -> do
            -- Test if all rules are satisfied
            return $ forAll (elements $ M.toList tileMap) $ \(pos, tile) -> 
                let (Rule rule) = rules tile
                    (result, _) = rule (TileMap tileMap) pos
                in resultToBool result

genTiles :: Gen [Tile]
genTiles = listOf1 genTile `suchThat` any (\(Tile _ (Rule rule) _) -> let (result, _) = rule (TileMap M.empty) (0, 0, 0) in resultToBool result)

-- | Generate a tile, which can be placed at any position
genTile :: Gen Tile
genTile = Tile <$> arbitrary <*> genRule <*> arbitrary

-- | Generate a rule, which can be placed at any position
genRule :: Gen Rule
genRule = genRuleResult >>= \result -> return $ Rule (\_ _ -> (result, []))

genRuleResult :: Gen RuleResult
genRuleResult = oneof [CanPlace <$> arbitrary, ChancePlace <$> (arbitrary `suchThat` (\f -> f >= 0 && f <= 1))]
