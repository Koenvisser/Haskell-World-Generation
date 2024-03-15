module Examples where

import Def
import Utils

redTile :: Tile
redTile = Tile {
    material = undefined,
    rules = (<!>) $ mustBeNextTo [blueTile, redTile, purpleTile, airTile] allNeighbours,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    material = undefined,
    rules = (<!>) $ mustBeNextTo [purpleTile, blueTile] allNeighbours,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    material = undefined,
    rules = (<!>) $ mustBeNextTo [purpleTile, redTile] allNeighbours,
    charRep = 'p'
}

airTile :: Tile
airTile = Tile {
    material = undefined,
    rules = (<!>) $ mustBeNextTo [] allNeighbours,
    charRep = 'a'
}

allTiles :: [Tile]
allTiles = [redTile, blueTile, purpleTile, airTile]