module ExamplesChance where

import Def
import Utils

redTile :: Tile
redTile = Tile {
    material = undefined,
    rules = (<!>) $ chanceRule 0.95,
    charRep = 'r'
}

blueTile :: Tile
blueTile = Tile {
    material = undefined,
    rules = (<||>) (chanceRule 0.0) $ chanceRule 0.1,
    charRep = 'b'
}

purpleTile :: Tile
purpleTile = Tile {
    material = undefined,
    rules = (<&&>) (chanceRule 0.1) $ chanceRule 0.1,
    charRep = 'p'
}

airTile :: Tile
airTile = Tile {
    material = undefined,
    rules = chanceRule 0.3,
    charRep = 'a'
}

allTiles :: [Tile]
allTiles = [redTile, blueTile, purpleTile, airTile]