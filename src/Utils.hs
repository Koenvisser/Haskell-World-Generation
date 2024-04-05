module Utils where

import Def

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


-- newtype Rule = Rule (Tile -> TileMap -> Pos -> RuleResult)
-- data RuleResult = CanPlace Bool | ChancePlace Float

-- | A rule that takes a list of tiles and a shape, and returns True if any of the tiles defined in the list
--   tile are within the shape at the given position. 
nextToAny :: [Tile] -> Shape -> Rule
nextToAny tiles shape = Rule (\(TileMap tileMap) pos -> 
    CanPlace <$> anyMonads (\nPos ->
      do 
        value <- lookupTile nPos (TileMap tileMap)
        case value of
            Just tile -> return $ tile `elem` tiles
            _ -> return False
        ) (shape pos))

-- | A rule that takes a list of tiles and a shape, and returns True if all of the tiles defined in the list
--   tile are within the shape at the given position. 
nextToAll :: [Tile] -> Shape -> Rule
nextToAll tiles shape = Rule (\(TileMap tileMap) pos -> 
    CanPlace <$> allMonads (\tile -> 
        anyMonads (\nPos ->
          do 
            value <- lookupTile nPos (TileMap tileMap)
            case value of
                Just tile' -> return $ tile == tile'
                _ -> return False
        ) (shape pos)) tiles)

-- | A rule that takes a list of tiles and a shape, and returns True if all of the tiles in the shape are 
--   within the list of tiles at the given position.
allMustBe :: [Tile] -> Shape -> Rule
allMustBe tiles shape = Rule (\tileMap pos -> 
  CanPlace <$> allMonads (\nPos -> 
    do 
      value <- lookupTile nPos tileMap
      case value of
          Just tile -> return $ tile `elem` tiles
          _ -> return True
      ) (shape pos))

anyMonads :: (a -> MonadTest Bool) -> [a] -> MonadTest Bool
anyMonads _ [] = return False
anyMonads f (m:ms) = do
  b <- f m
  if b then return True else anyMonads f ms

allMonads :: (a -> MonadTest Bool) -> [a] -> MonadTest Bool
allMonads _ [] = return True
allMonads f (m:ms) = do
  b <- f m
  if b then allMonads f ms else return False

-- | A rule that takes a float f and returns a rule with chance f of returning True 
weightedRule :: Float -> Rule
weightedRule chance = Rule (\_ _ -> return (ChancePlace chance))

-- | A function that takes a list of positions and returns a function that checks if a position is in the list
isInPos :: [Pos] -> (Pos -> Bool)
isInPos posList pos = pos `elem` posList

-- | A rule that takes a function that takes a position and returns a boolean, and returns a rule using this function
canExistAt :: (Pos -> Bool) -> Rule
canExistAt posPred = Rule (\_ pos -> return (CanPlace $ posPred pos))



-- | Must be next to
-- | Can not be next to
-- | (Overload with chance)

-- | Can not exist at coordinates
-- | Can only exist at coordinates
-- | (Overload with chance)
