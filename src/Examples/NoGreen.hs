-- | This world becomes impossible whenever a green tile is placed. This is a useful example 
--   to benchmark very difficult to create worlds and test backtracking.
module Examples.NoGreen where

import Def
import Utils
import Data.Default (def)

-- | A solid red material
redMaterial :: Material
redMaterial = def {diffuseColor = (1.0, 0, 0)}

-- | A tile that is red and can not be placed next to a green tile
redTile :: Tile
redTile = Tile {
  materials = createMaterialMapForAllSides redMaterial,
  rules = (<!>) $ nextToAny [greenTile] allNeighbours,
  charRep = 'r'
}

-- | A solid blue material
blueMaterial :: Material
blueMaterial = def {diffuseColor = (0, 0, 1.0)}

-- | A tile that is blue and can not be placed next to a green tile
blueTile :: Tile
blueTile = Tile {
  materials = createMaterialMapForAllSides blueMaterial,
  rules = (<!>) $ nextToAny [greenTile] allNeighbours,
  charRep = 'b'
}

-- | A solid green material
greenMaterial :: Material
greenMaterial = def {diffuseColor = (0, 1.0, 0)}

-- | A tile that is green and can not be placed next to a green tile
greenTile :: Tile
greenTile = Tile {
  materials = createMaterialMapForAllSides greenMaterial,
  rules = (<!>) $ nextToAny [greenTile] allNeighbours,
  charRep = 'g'
}

-- | The tiles that will be used in the wave function collapse algorithm to generate the world
allTiles :: [Tile]
allTiles = [greenTile, redTile, blueTile]
