module Def where

import Data.Default
import qualified Data.HashSet as H
import qualified Data.Map as M

-- | A tile is a 3D object with a texture, a set of rules and a character representation.
data Tile = Tile { 
    -- | The `FilePath` to a texture location, which is used in the `Output`.
    materials :: M.Map Side Material, 
    -- | The `Rule` that determines if a tile can be placed. 
    -- Rules can be composed using the functions from the `CompareRule` type class.
    rules :: Rule, 
    -- | The character representation is only used for debugging purposes. 
    charRep :: Char
}

data Material = Material {
    ambientColor :: (Float, Float, Float),
    diffuseColor :: (Float, Float, Float),
    specularColor :: (Float, Float, Float),
    transparency :: Float,
    specularExponent :: Float,
    illuminationModel :: Int,
    texture :: Maybe FilePath,
    extraFields :: [String],
    extraFiles :: [FilePath]
} deriving (Show, Eq, Ord)

data Side = PosX | NegX | PosY | NegY | PosZ | NegZ deriving (Show, Eq, Ord, Enum, Bounded)

instance Default Material where
    def = Material {
        ambientColor = (1.0, 1.0, 1.0),
        diffuseColor = (1.0, 1.0, 1.0),
        specularColor = (0.0, 0.0, 0.0),
        transparency = 1.0,
        specularExponent = 10.0,
        illuminationModel = 2,
        texture = Nothing,
        extraFields = [],
        extraFiles = []
    }

-- | The show instance of a tile is its character representation
instance Show Tile where
    show tile = [charRep tile]

-- | The Eq instance of a tile is based on its texture location, since no 
--   two tiles should have the same texture
instance Eq Tile where
    (==) tile1 tile2 = charRep tile1 == charRep tile2

-- | A rule is a function that takes a `TileMap` and a position and returns a `RuleResult`.
newtype Rule = Rule (TileMap -> Pos -> MonadTest RuleResult)

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

data MonadTest m = MonadTest m (H.HashSet Pos) deriving (Show, Eq, Ord)

getVal :: MonadTest a -> a
getVal (MonadTest a _) = a

getPos :: MonadTest a -> H.HashSet Pos
getPos (MonadTest _ pos) = pos

instance Functor MonadTest where
    fmap :: (a -> b) -> MonadTest a -> MonadTest b
    fmap f (MonadTest a pos) = MonadTest (f a) pos

instance Applicative MonadTest where
    pure :: a -> MonadTest a
    pure a = MonadTest a H.empty
    (<*>) :: MonadTest (a -> b) -> MonadTest a -> MonadTest b
    (MonadTest f pos1) <*> (MonadTest v pos2) = MonadTest (f v) (pos1 `H.union` pos2) 

instance Monad MonadTest where
    return :: a -> MonadTest a
    return = pure
    (>>=) :: MonadTest a -> (a -> MonadTest b) -> MonadTest b
    MonadTest v1 pos1 >>= f = let MonadTest v2 pos2 = f v1 in MonadTest v2 (pos1 `H.union` pos2)

instance Semigroup a => Semigroup (MonadTest a) where
    (<>) :: MonadTest a -> MonadTest a -> MonadTest a
    MonadTest v1 pos1 <> MonadTest v2 pos2 = MonadTest (v1 <> v2) (pos1 `H.union` pos2)

instance Monoid a => Monoid (MonadTest a) where
    mempty :: MonadTest a
    mempty = pure mempty
    mappend :: MonadTest a -> MonadTest a -> MonadTest a
    mappend = (<>)

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

-- | A position is a 3D coordinate in the world
type Pos = (Int, Int, Int)
-- | A size is a 3D coordinate representing the minimum and maximum coordinates of the world
type Size = (Pos, Pos)

-- | A tilemap is a map of positions to tiles in the world
newtype TileMap = TileMap (M.Map Pos Tile)

lookupTile :: Pos -> TileMap -> MonadTest (Maybe Tile)
lookupTile pos (TileMap tileMap) = MonadTest (M.lookup pos tileMap) (H.singleton pos)

-- | The show instance of a `TileMap` prints the the world as y slices of an x*z grid
--   with the tiles represented as their character representation and an empty tile 
--   represented as a space.
instance Show TileMap where
    show (TileMap tileMap) | M.null tileMap = "Empty tilemap"
                           | otherwise = unlines [unlines [[tileAtPos (x,y,z) | x <- [xMin..xMax]] | z <- [zMin..zMax]] |  y <- [yMin..yMax]]
        where
            (xMin, yMin, zMin) = fst . M.findMin $ tileMap
            (xMax, yMax, zMax) = fst . M.findMax $ tileMap
            tileAtPos :: Pos -> Char
            tileAtPos pos = case M.lookup pos tileMap of
                Just tile -> charRep tile
                _ -> ' '

-- | A shape is a function that takes a position and returns a list of absolute positions
--   that are relative to the given position, forming a shape. `Utils.allNeighbours` is 
--   an example of a shape.
type Shape = Pos -> [Pos]
