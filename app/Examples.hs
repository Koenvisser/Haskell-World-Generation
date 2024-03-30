module Examples where

import Def
import Utils

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
allTiles = [redTile, blueTile, purpleTile, airTile]