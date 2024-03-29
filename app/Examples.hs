module Examples where

import Def
import Utils

redTile :: Tile
redTile = Tile {
    textureLoc = "red.png",
    rules = (<!>) $ mustBeNextTo [blueTile, redTile, purpleTile, airTile] allNeighbours,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    textureLoc = "blue.png",
    rules = (<!>) $ mustBeNextTo [purpleTile, blueTile] allNeighbours,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    textureLoc = "purple.png",
    rules = (<!>) $ mustBeNextTo [purpleTile, redTile] allNeighbours,
    charRep = 'p'
}

airTile :: Tile
airTile = Tile {
    textureLoc = "air.png",
    rules = (<!>) $ mustBeNextTo [] allNeighbours,
    charRep = 'a'
}

allTiles :: [Tile]
allTiles = [redTile, blueTile, purpleTile, airTile]