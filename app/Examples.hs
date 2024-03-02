module Examples where

import Def
import Utils

redTile :: Tile
redTile = Tile {
    textureLoc = "red.png",
    rules = mustNotBeNextTo [blueTile, redTile, purpleTile, airTile] allNeighbours,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    textureLoc = "blue.png",
    rules = mustNotBeNextTo [purpleTile, blueTile] allNeighbours,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    textureLoc = "purple.png",
    rules = mustNotBeNextTo [purpleTile, redTile] allNeighbours,
    charRep = 'p'
}

airTile :: Tile
airTile = Tile {
    textureLoc = "air.png",
    rules = mustNotBeNextTo [] allNeighbours,
    charRep = 'a'
}

allTiles :: [Tile]
allTiles = [redTile, blueTile, purpleTile, airTile]