module Examples.Mountains (groundTiles, airTiles) where

import Data.Default (def)
import qualified Data.Map as M
import Def
import Utils

groundMaterial :: Material
groundMaterial = def {texture = Just "textures/side-dirt.png"}

airMaterial :: Material
airMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0.25}

waterMaterial :: Material
waterMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0.5}

groundTile :: Tile
groundTile = Tile {
    materials = M.fromList [(NegX, groundMaterial), (PosX, groundMaterial), (NegY, groundMaterial), (PosY, groundMaterial), (NegZ, groundMaterial), (PosZ, groundMaterial)],
    rules = weightedRule 0.8,
    charRep = 'g'
}

airTile :: Tile
airTile = Tile {
    materials = M.fromList [(NegX, airMaterial), (PosX, airMaterial), (NegY, airMaterial), (PosY, airMaterial), (NegZ, airMaterial), (PosZ, airMaterial)],
    rules = weightedRule 0.8,
    charRep = 'a'
}

groundTiles = [groundTile]
airTiles = [airTile]