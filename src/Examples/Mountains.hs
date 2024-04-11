module Examples.Mountains (groundTiles, airTiles) where

import Data.Default (def)
import qualified Data.Map as M
import Def
import Utils

-------------- Air tiles --------------
airMaterial :: Material
airMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0}

airTile :: Tile
airTile = Tile {
    materials = createSolidMaterialMap airMaterial,
    rules = weightedRule 0.5,
    charRep = 'a'
}

cloudMaterial :: Material
cloudMaterial = def {diffuseColor = (1.0, 1.0, 1.0), transparency = 0.7}

cloudTile :: Tile
cloudTile =
  Tile
    { materials = createSolidMaterialMap cloudMaterial,
      rules = weightedRule 0.05 <||> nextToAny [cloudTile] directNeighbours,
      charRep = 'c'
    }

-------------- Ground tiles --------------

groundMaterial :: Material
groundMaterial = def {texture = Just "textures/side-dirt.png"}

groundTile :: Tile
groundTile =
  Tile
    { materials = createSolidMaterialMap groundMaterial,
      rules = weightedRule 0.8,
      charRep = 'g'
    }

waterMaterial :: Material
waterMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0.5}

waterTile :: Tile
waterTile = Tile {
    materials = createSolidMaterialMap waterMaterial,
    rules = weightedRule 0.8,
    charRep = 'w'
}

groundTiles = [groundTile, waterTile]
airTiles = [airTile, cloudTile]