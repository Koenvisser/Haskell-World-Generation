module Def (
    Tile(..),
    Material(..),
    Side(..),
    Rule(..),
    RuleResult(..),
    resultToBool,
    resultToFloat,
    CompareRule(..),
    Pos,
    Size,
    TileMap,
    lookupTile,
    Shape,
    RuleMonad
) where

import qualified Data.Map as M

import Internal.Def (RuleMonad(..), Pos, Rule(..), RuleResult(..), Tile(..), Material(..), Side(..), TileMap(..))


-- | Converts a `RuleResult` to a boolean, which is true if the result is true or 
--   the chance that it is placed is greater than 0. 
resultToBool :: RuleResult -> Bool
resultToBool (CanPlace b) = b
resultToBool (ChancePlace f) = f > 0

-- | Convrts a `RuleResult` to a float, which returns the chance if it is a chance or
--   when it is a boolean it returns 1 for true and 0 for false. 
resultToFloat :: RuleResult -> Float
resultToFloat (CanPlace b) = if b then 1.0 else 0.0
resultToFloat (ChancePlace f) = f

class CompareRule a where
    -- | The OR operator for rules
    (<||>) :: a -> a -> a
    -- | The AND operator for rules
    (<&&>) :: a -> a -> a
    -- | The NOT operator for rules
    (<!>) :: a -> a

-- | The CompareRule instance for RuleResult follows regular boolean logic. The only 
--   notable instances are the `ChancePlace` instances. For `<||>` its calculated as the 
--   chance for either rule succeeding, for `<&&>` its the chance of both rules succeeding 
--   and `<!>` reverses the chance c by the calculation 1 - c.
instance CompareRule RuleResult where
    (CanPlace b1) <||> (CanPlace b2) = CanPlace (b1 || b2)
    (CanPlace True) <||> (ChancePlace _) = CanPlace True
    (CanPlace False) <||> (ChancePlace f) = ChancePlace f
    (ChancePlace _) <||> (CanPlace True) = CanPlace True
    (ChancePlace f) <||> (CanPlace False) = ChancePlace f
    (ChancePlace f1) <||> (ChancePlace f2) = ChancePlace $ max f1 f2
    (CanPlace b1) <&&> (CanPlace b2) = CanPlace (b1 && b2)
    (CanPlace True) <&&> (ChancePlace f) = ChancePlace f
    (CanPlace False) <&&> (ChancePlace _) = CanPlace False
    (ChancePlace f) <&&> (CanPlace True) = ChancePlace f
    (ChancePlace _) <&&> (CanPlace False) = CanPlace False
    (ChancePlace f1) <&&> (ChancePlace f2) = ChancePlace (f1 * f2)
    (<!>) (CanPlace b) = CanPlace (not b)
    (<!>) (ChancePlace f) = ChancePlace (1 - f)

-- | The CompareRule instance for Rule, which composes the rules with the given operator
instance CompareRule Rule where
    (Rule rule1) <||> (Rule rule2) = Rule (\tileMap pos -> 
        let result1 = rule1 tileMap pos 
            result2 = rule2 tileMap pos
        in (<||>) <$> result1 <*> result2)
    (Rule rule1) <&&> (Rule rule2) = Rule (\tileMap pos -> 
        let result1 = rule1 tileMap pos 
            result2 = rule2 tileMap pos
        in (<&&>) <$> result1 <*> result2)
    (<!>) (Rule rule) = Rule (\tileMap pos -> 
        let result = rule tileMap pos
        in (<!>) <$> result)

-- | A size is a 3D coordinate representing the minimum and maximum coordinates of the world
type Size = (Pos, Pos)

lookupTile :: Pos -> TileMap -> RuleMonad (Maybe Tile)
lookupTile pos (TileMap tileMap) = RuleMonad (M.lookup pos tileMap) [pos]

-- | A shape is a function that takes a position and returns a list of absolute positions
--   that are relative to the given position, forming a shape. `Utils.allNeighbours` is 
--   an example of a shape.
type Shape = Pos -> [Pos]
