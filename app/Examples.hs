module Examples where

import Def
import Utils
import Data.Default (def)
import qualified Data.Map as M

defaultMaterial :: M.Map Side Material
defaultMaterial = M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]

redTile :: Tile
redTile = Tile {
    materials = defaultMaterial,
    rules = (<!>) $ mustBeNextTo [purpleTile] allNeighbours,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    materials = defaultMaterial,
    rules = (<!>) $ mustBeNextTo [purpleTile] allNeighbours,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    materials = defaultMaterial,
    rules = (<!>) $ mustBeNextTo [purpleTile] allNeighbours,
    charRep = 'p'
}


allTiles :: [Tile]
allTiles = [purpleTile, redTile, blueTile]