module Utils where

import qualified Data.Map as M

import Def


listToShape :: [Pos] -> Shape
listToShape relPos (px, py, pz) = map (\(x, y, z) -> (x + px, y + py, z + pz)) relPos

allNeighbours :: Shape
allNeighbours = listToShape [(x, y, z) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x, y, z) /= (0, 0, 0)]


invertRule :: Rule -> Rule
invertRule (Rule rule) = Rule (\tileMap pos -> case rule tileMap pos of
    CanPlace b -> CanPlace (not b)
    ChancePlace f -> ChancePlace (1 - f))


-- newtype Rule = Rule (Tile -> TileMap -> Pos -> RuleResult)
-- data RuleResult = CanPlace Bool | ChancePlace Float

mustBeNextTo :: [Tile] -> Shape -> Rule
mustBeNextTo tiles shape = Rule (\(TileMap tileMap) pos -> 
    CanPlace $ any (\tile -> 
        case M.lookup pos tileMap of
            Just tile -> tile `elem` tiles
            _ -> False
        ) (shape pos))

mustNotBeNextTo :: [Tile] -> Shape -> Rule
mustNotBeNextTo tiles shape = invertRule $ mustBeNextTo tiles shape

-- | Must be next to
-- | Can not be next to
-- | (Overload with chance)

-- | Can not exist at coordinates
-- | Can only exist at coordinates
-- | (Overload with chance)
