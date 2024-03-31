module Examples where

import Def
import Utils
import Data.Default (def)
import qualified Data.Map as M

defaultMaterial :: M.Map Side Material
defaultMaterial = M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]

redTile :: Tile
redTile = Tile {
    materials = undefined,
    rules = (<!>) $ nextToAny [blueTile, redTile, purpleTile, airTile] allNeighbours,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    materials = undefined,
    rules = (<!>) $ nextToAny [purpleTile, blueTile] allNeighbours,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    materials = undefined,
    rules = (<!>) $ nextToAny [purpleTile, redTile] allNeighbours,
    charRep = 'p'
}

airTile :: Tile
airTile = Tile {
    materials = undefined,
    rules = (<!>) $ nextToAny [] allNeighbours,
    charRep = 'a'
}

allTiles :: [Tile]
allTiles = [purpleTile, redTile, blueTile]