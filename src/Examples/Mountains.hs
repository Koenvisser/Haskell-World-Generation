module Examples.Mountains (groundTiles, airTiles) where

import Data.Default (def)
import qualified Data.Map as M
import Def
import Utils

groundMaterial :: Material
groundMaterial = def {texture = Just "textures/side-dirt.png"}



waterMaterial :: Material
waterMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0.5}

groundTile :: Tile
groundTile = Tile {
    materials = M.fromList [(NegX, groundMaterial), (PosX, groundMaterial), (NegY, groundMaterial), (PosY, groundMaterial), (NegZ, groundMaterial), (PosZ, groundMaterial)],
    rules = weightedRule 0.8,
    charRep = 'g'
}

-------------- Air tiles --------------
airMaterial :: Material
airMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0}

airTile :: Tile
airTile = Tile {
    materials = M.fromList [(NegX, airMaterial), (PosX, airMaterial), (NegY, airMaterial), (PosY, airMaterial), (NegZ, airMaterial), (PosZ, airMaterial)],
    rules = weightedRule 0.8,
    charRep = 'a'
}

cloudMaterial :: Material
cloudMaterial = def {diffuseColor = (1.0, 1.0, 1.0), transparency = 0.4}

cloudTile :: Tile
cloudTile =
  Tile
    { materials = M.fromList [(NegX, cloudMaterial), (PosX, cloudMaterial), (NegY, cloudMaterial), (PosY, cloudMaterial), (NegZ, cloudMaterial), (PosZ, cloudMaterial)],
      rules = weightedRule 0.2,
      charRep = 'a'
    }


waterTile :: Tile
waterTile = Tile {
    materials = M.fromList [(NegX, waterMaterial), (PosX, waterMaterial), (NegY, waterMaterial), (PosY, waterMaterial), (NegZ, waterMaterial), (PosZ, waterMaterial)],
    rules = weightedRule 0.8,
    charRep = 'w'
}


groundTiles = [groundTile, waterTile]
airTiles = [airTile, cloudTile]