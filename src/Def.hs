module Def where

import qualified Data.Map as M
import Text.Show.Functions ()

-- TODO:
-- Generator fixen, miss example rules fixen
-- Add randomness to picking a tile and picking a position 


data Tile = Tile { 
    textureLoc :: String,
    rules :: Rule,
    charRep :: Char
} 

instance Show Tile where
    show tile = [charRep tile]

instance Eq Tile where
    (==) tile1 tile2 = textureLoc tile1 == textureLoc tile2


newtype Rule = Rule (TileMap -> Pos -> RuleResult)
data RuleResult = CanPlace Bool | ChancePlace Float

instance Monoid Rule where
    mempty = Rule (\_ _ -> CanPlace True)
    mappend (Rule rule1) (Rule rule2) = Rule (\tileMap pos -> rule1 tileMap pos <> rule2 tileMap pos)    

instance Semigroup Rule where
    (<>) = mappend

instance Monoid RuleResult where
    mempty = CanPlace True
    mappend (CanPlace b1) (CanPlace b2) = CanPlace (b1 && b2)
    mappend (CanPlace True) (ChancePlace f) = ChancePlace f
    mappend (CanPlace False) (ChancePlace f) = CanPlace False
    mappend (ChancePlace f) (CanPlace True) = ChancePlace f
    mappend (ChancePlace f) (CanPlace False) = CanPlace False
    mappend (ChancePlace f1) (ChancePlace f2) = ChancePlace (f1 * f2)

instance Semigroup RuleResult where
    (<>) = mappend

type Pos = (Int, Int, Int)
type Size = ((Int, Int, Int), (Int, Int, Int))

newtype World = World (Size, TileMap)
newtype TileMap = TileMap (M.Map Pos Tile)

instance Show TileMap where
    show (TileMap tileMap) = show $ World ((minPos, maxPos), TileMap tileMap)
        where
            minPos = fst . M.findMin $ tileMap
            maxPos = fst . M.findMax $ tileMap

instance Show World where
    show (World (((xMin, yMin, zMin), (xMax, yMax, zMax)), TileMap tileMap)) = unlines [unlines [[tileAtPos (x,y,z) | x <- [xMin..xMax]] | y <- [yMin..yMax]] | z <- [zMin..zMax]]
        where
            tileAtPos :: Pos -> Char
            tileAtPos pos = case M.lookup pos tileMap of
                Just tile -> charRep tile
                _ -> ' '

type Shape = Pos -> [Pos]



-- scuffedVisualise :: World -> IO ()
-- scuffedVisualise ((x, y, z), tileMap) =  

-- scuffedVisualiseSlice