-- | This module contains the definition of the tiles used in the mountain example.
--   This example creates a simple world with mountains, snow, rocks, and water.
--   The world is generated using a heightmap generated using perlin noise that uses 
--   two different tile sets (airTiles and groundTiles).
module WorldGen.Examples.Mountains (groundTiles, airTiles) where

import WorldGen.Def
import WorldGen.Utils
import Paths_Haskell_World_Generation (getDataFileName)

import Data.Default (def)
import qualified Data.Map as M

-------------- Air tiles --------------
-- | A material that represents air
airMaterial :: Material
airMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0}

-- | A tile that represents air, must be placed above 0.5 height of the world
airTile :: Tile
airTile = Tile {
  materials = createMaterialMapForAllSides airMaterial,
  rules = minRelativeHeight 0.5 <&&> weightedRule 0.2 <&&> (<!>) (allMustBe [cloudTile] directNeighbours False),
  charRep = 'a'
}

-- | A material that represents clouds
cloudMaterial :: Material
cloudMaterial = def {diffuseColor = (1.0, 1.0, 1.0), transparency = 0.6}

-- | A tile that represents clouds, must be placed above 0.85 height of the world. Has a higher chance of being placed
--   when next to another cloud tile.
cloudTile :: Tile
cloudTile = Tile { 
  materials = createMaterialMapForAllSides cloudMaterial,
  rules = minRelativeHeight 0.85 <&&> (weightedRule 0.005 <||> nextToAny [cloudTile] directNeighbours),
  charRep = 'c'
}

-- | A material that represents shallow water
shallowWaterMaterial :: Material
shallowWaterMaterial = def {diffuseColor = (0.1, 0.1, 1.0), transparency = 0.5}

-- | A tile that represents shallow water, must be placed below 0.5 height of the world, but above 0.3
shallowWaterTile :: Tile
shallowWaterTile = Tile {
  materials = createMaterialMapForAllSides shallowWaterMaterial,
  rules = maxRelativeHeight 0.5 <&&> minRelativeHeight 0.3,
  charRep = 'B'
}

-- | A material that represents deep water
deepWaterMaterial :: Material
deepWaterMaterial = def {diffuseColor = (0.1, 0.1, 0.6), transparency = 0.8}

-- | A tile that represents deep water, must be placed below 0.3 height of the world, so below the shallow water
deepWaterTile :: Tile
deepWaterTile = Tile {
  materials = createMaterialMapForAllSides deepWaterMaterial,
  rules = maxRelativeHeight 0.3,
  charRep = 'd'
}
    
-------------- Ground tiles --------------
-- | A solid white material that represents snow
snowMaterial :: Material
snowMaterial = def {diffuseColor = (1.0, 1.0, 1.0)}

-- | A tile that represents snow, must be placed above 0.8 and is used as a groundTile
snowTile :: Tile
snowTile = Tile { 
  materials = createMaterialMapForAllSides snowMaterial,
  rules = minRelativeHeight 0.8,
  charRep = 's'
}

-- | A solid grey material that represents rocks
rockMaterial :: Material
rockMaterial = def {diffuseColor = (0.5, 0.5, 0.5)}

-- | A tile that represents rocks, its weight scales with the relative height of the world up to 0.8.
rockTile :: Tile
rockTile = Tile { 
  materials = createMaterialMapForAllSides rockMaterial,
  rules = (minRelativeHeight 0.2 <&&> weightedRule 0.6 <&&> maxRelativeHeight 0.4) 
    <||> (maxRelativeHeight 0.2 <&&> (weightedRule 0.02 
    <||> (nextToAny [deepRockTile] directNeighbours <&&> weightedRule 0.1))) 
    <||> maxRelativeHeight 0.8 <&&> weightedRule 0.05,
  charRep = 'r'
}

-- | A solid dark-grey material that represents deep rocks
deepRockMaterial :: Material
deepRockMaterial = def {diffuseColor = (0.3, 0.3, 0.3)}

-- | A tile that represents rocks in the lower parts of the world, must be placed below 0.3 relative height of the world.
deepRockTile :: Tile
deepRockTile = Tile { 
  materials = createMaterialMapForAllSides deepRockMaterial,
  rules = maxRelativeHeight 0.3,
  charRep = 'R'
}

-- | A material that represents dirt, a simple dirt texture
dirtMaterial :: IO Material
dirtMaterial = do 
  tex <- getDataFileName "textures/side-dirt.png"
  return $ def {texture = Just tex}

-- | A material that represents grass, with a grass texture on top and dirt on the sides
grassBlock :: IO (M.Map Side Material)
grassBlock = do 
  tex <- getDataFileName "textures/top-grass.png"
  dirt <- dirtMaterial
  return $ M.fromList [ 
    (PosY, (def {texture = Just tex})),
    (NegY, dirt),
    (NegX, dirt),
    (PosX, dirt),
    (NegZ, dirt),
    (PosZ, dirt)
    ]

-- | A tile that represents grass, must be placed between 0.4 and 0.7 height of the world
grassTile :: IO Tile
grassTile = do 
  mat <- grassBlock
  return $ Tile { 
    materials = mat,
    rules = minRelativeHeight 0.4 <&&> maxRelativeHeight 0.7,
    charRep = 'g'
  }

-- | A tile that represents the ground, must be placed between 0.3 and 0.5 height of the world.
--   Uses the `dirtMaterial` for all sides.
groundTile :: IO Tile
groundTile = do 
  mat <- dirtMaterial
  return $ Tile { 
    materials = createMaterialMapForAllSides mat,
    rules = minRelativeHeight 0.3 <&&> maxRelativeHeight 0.5 <&&> weightedRule 0.05,
    charRep = 'd'
  }

-- | The tiles that will be placed __below__ the `HeightMap`
groundTiles :: IO [Tile]
groundTiles = do 
  ground <- groundTile
  grass <- grassTile  
  return [ground, grass, rockTile, snowTile, deepRockTile]

-- | The tiles that will be placed __above__ the `HeightMap`
airTiles :: IO [Tile]
airTiles = return [airTile, cloudTile, shallowWaterTile, deepWaterTile]
