module MyLib where

import qualified Data.Map as M
import Text.Show.Functions ()

-- TODO:
-- Split into files (example, datatypes)
-- Generalise some functions, e.g. notNextToNeighbour
-- Add randomness to picking a tile and picking a position


data Tile = Tile { 
    textureLoc :: String,
    isValid :: TileMap -> Pos -> Bool
} deriving (Show)

instance Eq Tile where
    (==) tile1 tile2 = textureLoc tile1 == textureLoc tile2

redTile :: Tile
redTile = Tile {
    textureLoc = "red.png",
    isValid = redValidFunc
} 

redValidFunc :: TileMap -> Pos -> Bool
redValidFunc tm pos = all pred (allNeighbours pos)
    where
        pred nPos = case M.lookup nPos tm of
                    Just tile -> tile /= blueTile
                    _ -> True

blueTile :: Tile
blueTile = Tile {
    textureLoc = "blue.png",
    isValid = blueValidFunc
} 

blueValidFunc :: TileMap -> Pos -> Bool
blueValidFunc tm pos = all pred (allNeighbours pos)
    where
        pred nPos = case M.lookup nPos tm of
                    Just tile -> tile /= redTile
                    _ -> True

purpleTile :: Tile
purpleTile = Tile {
    textureLoc = "purple.png",
    isValid = purpleValidFunc
} 

purpleValidFunc :: TileMap -> Pos -> Bool
purpleValidFunc tm pos = any redPred (allNeighbours pos) && any bluePred (allNeighbours pos)
    where
        redPred nPos = case M.lookup nPos tm of
                    Just tile -> tile == redTile
                    _ -> False
        bluePred nPos = case M.lookup nPos tm of
                    Just tile -> tile == blueTile
                    _ -> False

        


allNeighbours :: Pos -> [Pos]
allNeighbours (x, y, z) = [(x', y', z') | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], (x, y, z) /= (x', y', z')]

generate :: [Tile] -> TileMap
generate tiles = foldl (\tileMap (x, y, z) -> 
    let validTiles = filter (\tile -> isValid tile tileMap (x, y, z)) tiles
    in if not (null validTiles) then M.insert (x, y, z) (head validTiles) tileMap else tileMap
  ) M.empty [(x, y, z) | x <- [0..2], y <- [0..2], z <- [0..2]]

type Pos = (Int, Int, Int)

type TileMap = M.Map Pos Tile

someFunc :: IO ()
someFunc = putStrLn "someFunc"
