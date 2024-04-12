-- | This module contains the definition of the tiles used in the mountain example.
--   This example creates a simple world with mountains, snow, rocks, and water.
--   The world is generated using a heightmap generated using perlin noise that uses 
--   two different tile sets (airTiles and groundTiles).
module Examples.Mountains (groundTiles, airTiles) where

import Data.Default (def)
import Def
import Utils
import qualified Data.Map as M

-------------- Air tiles --------------
airMaterial :: Material
airMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0}

airTile :: Tile
airTile = Tile {
  materials = createMaterialMapForAllSides airMaterial,
  rules = minRelativeHeight 0.5 <&&> weightedRule 0.2 <&&> (<!>) (allMustBe [cloudTile] directNeighbours False),
  charRep = 'a'
}

cloudMaterial :: Material
cloudMaterial = def {diffuseColor = (1.0, 1.0, 1.0), transparency = 0.6}

cloudTile :: Tile
cloudTile = Tile { 
  materials = createMaterialMapForAllSides cloudMaterial,
  rules = minRelativeHeight 0.85 <&&> (weightedRule 0.005 <||> nextToAny [cloudTile] directNeighbours),
  charRep = 'c'
}

shallowWaterMaterial :: Material
shallowWaterMaterial = def {diffuseColor = (0.1, 0.1, 1.0), transparency = 0.5}

shallowWaterTile :: Tile
shallowWaterTile = Tile {
  materials = createMaterialMapForAllSides shallowWaterMaterial,
  rules = maxRelativeHeight 0.5 <&&> minRelativeHeight 0.3,
  charRep = 'B'
}

deepWaterMaterial :: Material
deepWaterMaterial = def {diffuseColor = (0.1, 0.1, 0.6), transparency = 0.8}

deepWaterTile :: Tile
deepWaterTile = Tile {
  materials = createMaterialMapForAllSides deepWaterMaterial,
  rules = maxRelativeHeight 0.3,
  charRep = 'd'
}
    
-------------- Ground tiles --------------

snowMaterial :: Material
snowMaterial = def {diffuseColor = (1.0, 1.0, 1.0)}

snowTile :: Tile
snowTile = Tile { 
  materials = createMaterialMapForAllSides snowMaterial,
  rules = minRelativeHeight 0.8,
  charRep = 's'
}

rockMaterial :: Material
rockMaterial = def {diffuseColor = (0.5, 0.5, 0.5)}

rockTile :: Tile
rockTile = Tile { 
  materials = createMaterialMapForAllSides rockMaterial,
  rules = (minRelativeHeight 0.2 <&&> weightedRule 0.6 <&&> maxRelativeHeight 0.4) 
    <||> (maxRelativeHeight 0.2 <&&> (weightedRule 0.02 
    <||> (nextToAny [deepRockTile] directNeighbours <&&> weightedRule 0.1))) 
    <||> maxRelativeHeight 0.8 <&&> weightedRule 0.05,
  charRep = 'r'
}

deepRockMaterial :: Material
deepRockMaterial = def {diffuseColor = (0.3, 0.3, 0.3)}

deepRockTile :: Tile
deepRockTile = Tile { 
  materials = createMaterialMapForAllSides deepRockMaterial,
  rules = maxRelativeHeight 0.3,
  charRep = 'R'
}

dirtMaterial :: Material
dirtMaterial = def {texture = Just "textures/side-dirt.png"}

grassBlock :: M.Map Side Material
grassBlock = M.fromList [ 
  (PosY, (def {texture = Just "textures/top-grass.png"})),
  (NegY, dirtMaterial),
  (NegX, dirtMaterial),
  (PosX, dirtMaterial),
  (NegZ, dirtMaterial),
  (PosZ, dirtMaterial)
  ]

grassTile :: Tile
grassTile = Tile { 
  materials = grassBlock,
  rules = minRelativeHeight 0.4 <&&> maxRelativeHeight 0.7,
  charRep = 'g'
}

groundMaterial :: Material
groundMaterial = def {texture = Just "textures/side-dirt.png"}

groundTile :: Tile
groundTile = Tile { 
  materials = createMaterialMapForAllSides groundMaterial,
  rules = minRelativeHeight 0.3 <&&> maxRelativeHeight 0.5 <&&> weightedRule 0.05,
  charRep = 'd'
}

-- | The tiles that will be placed __below__ the heightmap
groundTiles :: [Tile]
groundTiles = [groundTile, grassTile, rockTile, snowTile, deepRockTile]

-- | The tiles that will be placed __above__ the heightmap
airTiles :: [Tile]
airTiles = [airTile, cloudTile, shallowWaterTile, deepWaterTile]
