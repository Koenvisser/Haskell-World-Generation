-- | This module contains examples of tiles that have a chance of being generated using the weightedRule.
module WorldGen.Examples.Chance where

import WorldGen.Def
import WorldGen.Utils

import Data.Default (def)

-- | A solid red material
redMaterial :: Material
redMaterial = def {diffuseColor = (1.0, 0, 0)}

-- | A red tile that negates a weighted rule (<!> 0.90), effectively making the rule have a 10% chance of being true 
redTile :: Tile
redTile = Tile {
  materials = createMaterialMapForAllSides redMaterial,
  rules = (<!>) $ weightedRule 0.90,
  charRep = 'r'
}

-- | A solid blue material
blueMaterial :: Material
blueMaterial = def {diffuseColor = (0, 0, 1.0)}

-- | A blue tile that combines two weighted rules with an `<&&>` operator, effectively multiplying the chances of both rules
blueTile :: Tile
blueTile = Tile {
  materials = createMaterialMapForAllSides blueMaterial,
  rules = weightedRule 0.3 <&&> weightedRule 0.3, 
  charRep = 'b'
}

-- | A solid green material
greenMaterial :: Material
greenMaterial = def {diffuseColor = (0, 1.0, 0)}

-- | A green tile that combines two weighted rules with an `<||>` operator, effectively adding the chances of both rules 
--   together
greenTile :: Tile
greenTile = Tile {
  materials = createMaterialMapForAllSides greenMaterial,
  rules = weightedRule 0.1 <||> weightedRule 0.001,
  charRep = 'g'
}

-- | A transparent material that represents air
airMaterial :: Material
airMaterial = def {transparency = 0.0}

-- | A textureless air tile that has a 30% chance of being placed
airTile :: Tile
airTile = Tile {
  materials = createMaterialMapForAllSides airMaterial,
  rules = weightedRule 0.3,
  charRep = 'a'
}

-- | The tiles that will be used in the wave function collapse algorithm to generate the world
allTiles :: [Tile]
allTiles = [redTile, blueTile, greenTile, airTile]
