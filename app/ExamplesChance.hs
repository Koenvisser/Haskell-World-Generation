module ExamplesChance where

import Def
import Utils

redTile :: Tile
redTile = Tile {
    materials = undefined,
    rules = (<!>) $ weightedRule 0.95,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    materials = undefined,
    rules = (<||>) (weightedRule 0.0) $ weightedRule 0.1,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    materials = undefined,
    rules = (<&&>) (weightedRule 0.1) $ weightedRule 0.1,
    charRep = 'p'
}

airTile :: Tile
airTile = Tile {
    materials = undefined,
    rules = weightedRule 0.3,
    charRep = 'a'
}

allTiles :: [Tile]
allTiles = [redTile, blueTile, purpleTile, airTile]