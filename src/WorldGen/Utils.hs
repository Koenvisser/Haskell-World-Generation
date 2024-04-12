-- | This module contains utility functions and data structures that can be used to define rules
--   in an easier way. Also contains functions that can be used to create shapes that can be used
--   in the rules, and a function that can be used to create a simple material map for all sides.
module WorldGen.Utils where

import WorldGen.Def
import qualified Data.Map as M

-- | Convert a list of relative positions into a shape that takes a position and returns
--   the absolute positions of the shape at that position
listToShape :: [Pos] -> Shape
listToShape relPos (px, py, pz) = map (\(x, y, z) -> (x + px, y + py, z + pz)) relPos

-- | A shape that contains all the direct horizontal, vertical and diagonal neighbours of a position
allNeighbours :: Shape
allNeighbours = listToShape [(x, y, z) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x, y, z) /= (0, 0, 0)]

directNeighbours :: Shape
directNeighbours = listToShape [(-1, 0, 0), (1, 0, 0), (0, 0, 1), (0, 0, -1), (0, 1, 0), (0, -1, 0)]

-- | A shape that contains the neighbour to the left of a position
leftNeighbour :: Shape
leftNeighbour = listToShape [(-1, 0, 0)]

-- | A shape that contains the neighbour to the right of a position
rightNeighbour :: Shape
rightNeighbour = listToShape [(1, 0, 0)]

-- | A shape that contains the neighbour to the front of a position
downNeighbour :: Shape
downNeighbour = listToShape [(0, 0, 1)]

-- | A shape that contains the neighbour to the back of a position
upNeighbour :: Shape
upNeighbour = listToShape [(0, 0, -1)]

-- | A shape that contains the neighbour above a position
aboveNeighbour :: Shape
aboveNeighbour = listToShape [(0, 1, 0)]

-- | A shape that contains the neighbour below a position
belowNeighbour :: Shape
belowNeighbour = listToShape [(0, -1, 0)]

-- | A rule that takes a list of tiles and a shape, and returns True if any of the tiles defined in the list
--   tile are within the shape at the given position. 
nextToAny :: [Tile] -> Shape -> Rule
nextToAny tiles shape = Rule (\tileMap pos -> 
  CanPlace <$> anyRule (\nPos ->
    do 
      value <- lookupTileMap nPos tileMap
      case value of
          Just tile -> return $ tile `elem` tiles
          _ -> return False
      ) (shape pos))

-- | A rule that takes a list of tiles and a shape, and returns True if all of the tiles defined in the list
--   tile are within the shape at the given position. 
nextToAll :: [Tile] -> Shape -> Rule
nextToAll tiles shape = Rule (\tileMap pos -> 
  CanPlace <$> allRule (\tile -> 
      anyRule (\nPos ->
        do 
          value <- lookupTileMap nPos tileMap
          case value of
              Just tile' -> return $ tile == tile'
              _ -> return False
      ) (shape pos)) tiles)

-- | A rule that takes a list of tiles and a shape, and returns True if all of the tiles in the shape are 
--   within the list of tiles at the given position. The boolean parameter determines if an empty tile 
--   should be considered a match.
allMustBe :: [Tile] -- ^ List of tiles that must be present 
          -> Shape -- ^ Shape that must contain the tiles
          -> Bool -- ^ If an empty tile should be considered a match
          -> Rule
allMustBe tiles shape notPlaced = Rule (\tileMap pos -> 
  CanPlace <$> allRule (\nPos -> 
    do 
      value <- lookupTileMap nPos tileMap
      case value of
          Just tile -> return $ tile `elem` tiles
          _ -> return notPlaced
      ) (shape pos))

-- | A rule that applies a function to a list of rules, and returns True if the function returns True for any of the rules
anyRule :: (a -> RuleMonad Bool) -> [a] -> RuleMonad Bool
anyRule _ [] = return False
anyRule f (m:ms) = (||) <$> f m <*> anyRule f ms 

-- | A rule that applies a function to a list of rules, and returns True if the function returns True for all of the rules
allRule :: (a -> RuleMonad Bool) -> [a] -> RuleMonad Bool
allRule _ [] = return True
allRule f (m:ms) = (&&) <$> f m <*> allRule f ms

-- | A rule that takes a float f and returns a rule with chance f of returning True 
weightedRule :: Float -> Rule
weightedRule chance = Rule (\_ _ -> return (ChancePlace chance))

-- | A rule that checks if the position has a height of at most p times the height of the tileMap
maxRelativeHeight :: Float -> Rule
maxRelativeHeight p = Rule (\tileMap (_, y, _) -> let 
  ((_, minY, _),(_, maxY, _)) = getSize tileMap
  yLimit = fromIntegral minY + p * (fromIntegral maxY - fromIntegral minY)
  in return (CanPlace $ yLimit >= fromIntegral y))

-- | A rule that checks if the position has a height of at least p times the height of the tileMap
minRelativeHeight :: Float -> Rule
minRelativeHeight p = Rule (\tileMap (_, y, _) -> let 
  ((_, minY, _),(_, maxY, _)) = getSize tileMap
  yLimit = fromIntegral minY + p * (fromIntegral maxY - fromIntegral minY)
  in return (CanPlace $ yLimit <= fromIntegral y))

-- | A function that takes a list of positions and returns a function that checks if a position is in the list
isInPos :: [Pos] -> (Pos -> Bool)
isInPos posList pos = pos `elem` posList

-- | A rule that takes a function that takes a position and returns a boolean, and returns a rule using this function
canExistAt :: (Pos -> Bool) -> Rule
canExistAt posPred = Rule (\_ pos -> return (CanPlace $ posPred pos))

-- | A function that takes a material and returns a map from all sides to that material
createMaterialMapForAllSides :: Material -> M.Map Side Material
createMaterialMapForAllSides material = M.fromList [(NegX, material), (PosX, material), (NegY, material), (PosY, material), (NegZ, material), (PosZ, material)]
