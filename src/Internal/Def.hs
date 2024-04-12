-- | This module defines the internal types and classes used in the library.
--   It is recommended to Do not import this module directly, instead import "Def" which re-exports
--   most of the types and classes defined here.
--   Only import this module if you know what you are doing, as some of the types and classes defined here 
--   can break the generator if used incorrectly.

module Internal.Def (
    Pos,
    RuleMonad(..),
    getVal,
    getPos,
    Rule(..),
    RuleResult(..),
    Tile(..),
    Material(..),
    Side(..),
    Size,
    TileMap(..),
    getMap
) where

import Data.Default
import Data.List (union)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | A position is a 3D coordinate in the world
type Pos = (Int, Int, Int)

-- | A rule monad is a monad that is used to compose rules and keep track of the positions that are used in the rule.
data RuleMonad m = RuleMonad m [Pos] deriving (Show, Eq)

-- | Get the value of a `RuleMonad`. Only use this function if the result will not be used in the generator.
getVal :: RuleMonad a -> a
getVal (RuleMonad a _) = a

-- | Get the positions that are used in the `RuleMonad`. This is used to keep track of the positions that are used in the rule.
getPos :: RuleMonad a -> [Pos]
getPos (RuleMonad _ pos) = pos

instance Functor RuleMonad where
    fmap :: (a -> b) -> RuleMonad a -> RuleMonad b
    fmap f (RuleMonad a pos) = RuleMonad (f a) pos

instance Applicative RuleMonad where
    pure :: a -> RuleMonad a
    pure a = RuleMonad a []
    (<*>) :: RuleMonad (a -> b) -> RuleMonad a -> RuleMonad b
    (RuleMonad f pos1) <*> (RuleMonad v pos2) = RuleMonad (f v) (pos1 `union` pos2) 

instance Monad RuleMonad where
    return :: a -> RuleMonad a
    return = pure
    (>>=) :: RuleMonad a -> (a -> RuleMonad b) -> RuleMonad b
    RuleMonad v1 pos1 >>= f = let RuleMonad v2 pos2 = f v1 in RuleMonad v2 (pos1 `union` pos2)

instance Semigroup a => Semigroup (RuleMonad a) where
    (<>) :: RuleMonad a -> RuleMonad a -> RuleMonad a
    RuleMonad v1 pos1 <> RuleMonad v2 pos2 = RuleMonad (v1 <> v2) (pos1 `union` pos2)

instance Monoid a => Monoid (RuleMonad a) where
    mempty :: RuleMonad a
    mempty = pure mempty
    mappend :: RuleMonad a -> RuleMonad a -> RuleMonad a
    mappend = (<>)

-- | A rule is a function that takes a `TileMap` and a position and returns a `RuleResult`.
newtype Rule = Rule (TileMap -> Pos -> RuleMonad RuleResult) deriving (Generic, NFData)

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

-- | A tile is a 3D object with a texture, a set of rules and a character representation.
data Tile = Tile { 
    -- | The `FilePath` to a texture location, which is used in the `Output`.
    materials :: M.Map Side Material, 
    -- | The `Rule` that determines if a tile can be placed. 
    -- Rules can be composed using the functions from the `CompareRule` type class.
    rules :: Rule, 
    -- | The character representation is only used for debugging purposes. 
    charRep :: Char
} deriving (Generic, NFData)

-- | A material is a set of properties that are used to render the tile in a 3D renderer.
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
} deriving (Show, Eq, Ord, Generic, NFData)

-- | A side is a direction in the world, which is used to determine the material of a tile.
data Side = PosX | NegX | PosY | NegY | PosZ | NegZ deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

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

-- | A size is a 3D coordinate representing the minimum and maximum coordinates of the world
type Size = (Pos, Pos)

-- | A tilemap is a map of positions to tiles in the world
newtype TileMap = TileMap (M.Map Pos Tile, Size) deriving (Generic, NFData)

-- | Return the underlying map of a `TileMap`. Only use this function if the map will not be used in the generator. 
getMap :: TileMap -> M.Map Pos Tile
getMap (TileMap (tileMap, _)) = tileMap

-- | The show instance of a `TileMap` prints the the world as y slices of an x*z grid
--   with the tiles represented as their character representation and an empty tile 
--   represented as a space.
instance Show TileMap where
    show (TileMap (tileMap, ((xMin, yMin, zMin),(xMax, yMax, zMax))))
        | M.null tileMap = "Empty tilemap"
        | otherwise = unlines [unlines [[tileAtPos (x,y,z) | x <- [xMin..xMax]] | z <- [zMin..zMax]] |  y <- [yMin..yMax]]
        where
            tileAtPos :: Pos -> Char
            tileAtPos pos = case M.lookup pos tileMap of
                Just tile -> charRep tile
                _ -> ' '
