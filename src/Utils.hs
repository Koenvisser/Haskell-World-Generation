module Utils where

import qualified Data.Map as M

import Def

-- | Convert a list of relative positions into a shape that takes a position and returns
--   the absolute positions of the shape at that position
listToShape :: [Pos] -> Shape
listToShape relPos (px, py, pz) = map (\(x, y, z) -> (x + px, y + py, z + pz)) relPos

-- | A shape that contains all the direct horizontal, vertical and diagonal neighbours of a position
allNeighbours :: Shape
allNeighbours = listToShape [(x, y, z) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x, y, z) /= (0, 0, 0)]

-- newtype Rule = Rule (Tile -> TileMap -> Pos -> RuleResult)
-- data RuleResult = CanPlace Bool | ChancePlace Float

-- | A rule that takes a list of tiles and a shape, and returns True if any of the tiles defined in the list
--   tile are within the shape at the given position. 
nextToAny :: [Tile] -> Shape -> Rule
nextToAny tiles shape = Rule (\(TileMap tileMap) pos -> 
    CanPlace $ any (\nPos -> 
        case M.lookup nPos tileMap of
            Just tile -> tile `elem` tiles
            _ -> False
        ) (shape pos))

-- | A rule that takes a list of tiles and a shape, and returns True if all of the tiles defined in the list
--   tile are within the shape at the given position. 
nextToAll :: [Tile] -> Shape -> Rule
nextToAll tiles shape = Rule (\(TileMap tileMap) pos -> 
    CanPlace $ all (\nPos -> 
        case M.lookup nPos tileMap of
            Just tile -> tile `elem` tiles
            _ -> False
        ) (shape pos))



-- | A rule that takes a float f and returns a rule with chance f of returning True 
chanceRule :: Float -> Rule
chanceRule chance = Rule (\_ _ -> ChancePlace chance)

-- | A function that takes a list of positions and returns a function that checks if a position is in the list
isInPos :: [Pos] -> (Pos -> Bool)
isInPos posList pos = pos `elem` posList

-- | A rule that takes a function that takes a position and returns a boolean, and returns a rule using this function
canExistAt :: (Pos -> Bool) -> Rule
canExistAt posPred = Rule (\_ pos -> CanPlace $ posPred pos)



-- | Must be next to
-- | Can not be next to
-- | (Overload with chance)

-- | Can not exist at coordinates
-- | Can only exist at coordinates
-- | (Overload with chance)
