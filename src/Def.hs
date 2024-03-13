module Def where

import qualified Data.Map as M

-- | A tile is a 3D object with a texture, a set of rules and a character representation.
--   The texture is a file path to the texture of the tile.
--   The rules are used to determine if a tile can be placed at a certain position.
--   The character representation is only used for debugging purposes.
data Tile = Tile { 
    -- | The `FilePath` to a texture location, which is used in the `Output`.
    textureLoc :: FilePath, 
    -- | The `Rule` that determines if a tile can be placed. 
    -- Rules can be composed using the functions from the `CompareRule` type class.
    rules :: Rule, 
    -- | The character representation is only used for debugging purposes. 
    charRep :: Char
}

-- | The show instance of a tile is its character representation
instance Show Tile where
    show tile = [charRep tile]

-- | The Eq instance of a tile is based on its texture location, since no 
--   two tiles should have the same texture
instance Eq Tile where
    (==) tile1 tile2 = textureLoc tile1 == textureLoc tile2

-- | A rule is a function that takes a `TileMap` and a position and returns a `RuleResult`.
newtype Rule = Rule (TileMap -> Pos -> RuleResult)

-- | The result of a rule is defined as a `RuleResult`. It is represented either as a 
--   `CanPlace Bool` which means the rule guaranteed passes or fails. Or as 
--   `ChancePlace Float` which gives a chance between 0 and 1 that the rule passes (1 being 100%).
data RuleResult 
    -- | `CanPlace` simply specifies if a tile can be placed at a position or not. When it is true,
    --   it has weight 1, when it is false it has weight 0
    = CanPlace Bool 
    -- | `ChancePlace` specifies the chance that a tile can be placed at a position. If it is 0, it will
    --   never be placed. Otherwise, it represents the weight that is used in the generator.
    | ChancePlace Float

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
    (ChancePlace f1) <||> (ChancePlace f2) = ChancePlace ((1.0 + f1) * (1.0 + f2) - 1.0)
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
    (Rule rule1) <||> (Rule rule2) = Rule (\tileMap pos -> rule1 tileMap pos <||> rule2 tileMap pos)
    (Rule rule1) <&&> (Rule rule2) = Rule (\tileMap pos -> rule1 tileMap pos <&&> rule2 tileMap pos)
    (<!>) (Rule rule) = Rule (\tileMap pos -> (<!>) (rule tileMap pos))

-- | A position is a 3D coordinate in the world
type Pos = (Int, Int, Int)
-- | A size is a 3D coordinate representing the minimum and maximum coordinates of the world
type Size = ((Int, Int, Int), (Int, Int, Int))

-- | A world is a 3D grid of tiles, represented as a size of the world 
--   and a tilemap containing the tiles at each position in the world
newtype World = World (Size, TileMap)

-- | A tilemap is a map of positions to tiles in the world
newtype TileMap = TileMap (M.Map Pos Tile)

-- | The show instance of a tilemap finds it's x and y boundaries and uses these to 
--   define and use the show instance of a world.
instance Show TileMap where
    show (TileMap tileMap) = show $ World ((minPos, maxPos), TileMap tileMap)
        where
            minPos = fst . M.findMin $ tileMap
            maxPos = fst . M.findMax $ tileMap

-- | The show instance of a world prints the the world as y slices of an x*z grid
--   with the tiles represented as their character representation and an empty tile 
--   represented as a space.
instance Show World where
    show (World (((xMin, yMin, zMin), (xMax, yMax, zMax)), TileMap tileMap)) = unlines [unlines [[tileAtPos (x,y,z) | x <- [xMin..xMax]] | y <- [yMin..yMax]] | z <- [zMin..zMax]]
        where
            tileAtPos :: Pos -> Char
            tileAtPos pos = case M.lookup pos tileMap of
                Just tile -> charRep tile
                _ -> ' '

-- | A shape is a function that takes a position and returns a list of absolute positions
--   that are relative to the given position, forming a shape. `Utils.allNeighbours` is 
--   an example of a shape.
type Shape = Pos -> [Pos]
