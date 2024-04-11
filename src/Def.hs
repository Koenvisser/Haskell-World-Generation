-- | This module defines the types and functions that are used in the rules and the
--   generation of the world. Use this module to define the tiles and the rules that
--   are used to generate the world, or use the "Utils" module to define your rules
--   in an easier way, using the provided functions.
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
    lookupTileMap,
    memberTileMap,
    getTileMap,
    nullTileMap,
    findWithDefaultTileMap,
    getSize,
    Shape,
    Error,
    RuleMonad
) where

import qualified Data.Map as M
import Data.List (union)

import Internal.Def (RuleMonad(..), Pos, Rule(..), RuleResult(..), Tile(..), Material(..), Side(..), TileMap(..), Size)

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

-- | The CompareRule typeclass is used to compare rules with each other. It is used to
--   compose rules with the operators <||>, <&&> and <!>.
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

instance CompareRule a => CompareRule (RuleMonad a) where
    (RuleMonad r1 pos1) <||> (RuleMonad r2 pos2) = RuleMonad (r1 <||> r2) (pos1 `union` pos2)
    (RuleMonad r1 pos1) <&&> (RuleMonad r2 pos2) = RuleMonad (r1 <&&> r2) (pos1 `union` pos2)
    (<!>) (RuleMonad r pos) = RuleMonad ((<!>) r) pos

-- | The CompareRule instance for Rule, which composes the rules with the given operator
instance CompareRule Rule where
    (Rule rule1) <||> (Rule rule2) = Rule (\tileMap pos -> 
        let result1 = rule1 tileMap pos 
            result2 = rule2 tileMap pos
        in result1 <||> result2)
    (Rule rule1) <&&> (Rule rule2) = Rule (\tileMap pos -> 
        let result1 = rule1 tileMap pos 
            result2 = rule2 tileMap pos
        in result1 <&&> result2)
    (<!>) (Rule rule) = Rule (\tileMap pos -> 
        let result = rule tileMap pos
        in (<!>) result)

-- | Looks up a tile in the tilemap at the given position, returning the tile if it exists
lookupTileMap :: Pos -> TileMap -> RuleMonad (Maybe Tile)
lookupTileMap pos (TileMap (tileMap, _)) = RuleMonad (M.lookup pos tileMap) [pos]

-- | Checks if a tile exists in the tilemap at the given position
memberTileMap :: Pos -> TileMap -> RuleMonad Bool
memberTileMap pos (TileMap (tileMap, _)) = RuleMonad (M.member pos tileMap) [pos]

-- | Gets a tile in the tilemap at the given position, returning an error if it does not exist
getTileMap :: Pos -> TileMap -> RuleMonad Tile
getTileMap pos (TileMap (tileMap, _)) = case M.lookup pos tileMap of
    Just tile -> RuleMonad tile [pos]
    Nothing -> RuleMonad (error "Tile not found") [pos]

-- | Checks if the tilemap is empty
nullTileMap :: TileMap -> Bool
nullTileMap (TileMap (tileMap, _)) = M.null tileMap

-- | Looks up a tile in the tilemap at the given position, returning the default tile if it does not exist
findWithDefaultTileMap :: Tile -> Pos -> TileMap -> RuleMonad Tile
findWithDefaultTileMap def pos (TileMap (tileMap, _)) = RuleMonad (M.findWithDefault def pos tileMap) [pos]

getSize :: TileMap -> Size
getSize (TileMap (_, size)) = size

-- | A shape is a function that takes a position and returns a list of absolute positions
--   that are relative to the given position, forming a shape. `Utils.allNeighbours` is 
--   an example of a shape.
type Shape = Pos -> [Pos]

-- | Represents an error message that can be thrown in the generator
type Error = String
