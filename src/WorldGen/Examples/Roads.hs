-- | A module that defines a set of tiles that can be used to generate a road-like structure.
--   The roads must be connected to each other, and the road can have a turn, a split, a cross, a straight line, or an elbow.
--   The world can also contain dirt, grass and water.
module WorldGen.Examples.Roads where

import WorldGen.Def
import WorldGen.Utils

import Data.Default (def)
import qualified Data.Map as M

-- | The material that will be used to represent dirt, uses a dirt texture
dirtMaterial :: Material
dirtMaterial = def {texture = Just "textures/side-dirt.png"}

-- | The material that will be used to represent grass, uses a grass texture
grassDirtMaterialSide :: Material
grassDirtMaterialSide = def {texture = Just "textures/side-dirt-grass.png"}

-- | The material map  that will be used to represent a top level block, the PosY still has to be defined
topLevelBlock :: M.Map Side Material
topLevelBlock = M.fromList [
  (NegY, dirtMaterial),
  (NegX, grassDirtMaterialSide),
  (PosX, grassDirtMaterialSide),
  (NegZ, grassDirtMaterialSide),
  (PosZ, grassDirtMaterialSide)
  ]

-- | The material map that will be used to represent a dirt block, the PosY is already defined as dirtMaterial
dirtBlock :: M.Map Side Material
dirtBlock = createMaterialMapForAllSides dirtMaterial

-- | The material map that will be used to represent a water block, it is defined as a semi-transparent solid blue block
waterMaterial :: Material
waterMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0.5}

-- | The material map that will be used to represent a water block
waterBlock :: M.Map Side Material
waterBlock = createMaterialMapForAllSides waterMaterial

-- | A rule that checks if the tile has y = 3
posTopRule :: Rule
posTopRule = canExistAt (\(_, y, _) -> y == 3)

-- | A shape that checks the positions of +z and -z
verticalNeighbour :: Shape
verticalNeighbour = listToShape [(0, 0, -1), (0, 0, 1)]

-- | A shape that checks the positions of +x and -x
horizontalNeighbour :: Shape
horizontalNeighbour = listToShape [(-1, 0, 0), (1, 0, 0)]

-- | A list of all tiles that have a left connection
leftConnection :: [Tile]
leftConnection = [
  topCrossTile, 
  topBLUTSplitTile, 
  topHorizontalPipeTile,
  topLURTSplitTile,
  topLRBTSplitTile,
  topLDElbowTile,
  topULElbowTile
  ]

-- | A list of all tiles that have a right connection
rightConnection :: [Tile]
rightConnection = [
  topCrossTile, 
  topHorizontalPipeTile, 
  topURBTSplitTile,
  topLRBTSplitTile,
  topLURTSplitTile,
  topDRElbowTile,
  topURElbowTile
  ]

-- | A list of all tiles that have an up connection
upConnection :: [Tile]
upConnection = [
  topCrossTile, 
  topBLUTSplitTile,
  topLURTSplitTile,
  topURBTSplitTile,
  topVerticalPipeTile,
  topURElbowTile,
  topULElbowTile
  ]

-- | A list of all tiles that have a down connection
downConnection :: [Tile]
downConnection = [
  topCrossTile, 
  topBLUTSplitTile,
  topURBTSplitTile,
  topLRBTSplitTile, 
  topVerticalPipeTile,
  topDRElbowTile,
  topLDElbowTile
  ]

-- | A rule that forces the neighbour above this to have a downConnection
upConnectionRule :: Rule
upConnectionRule = allMustBe downConnection upNeighbour True

-- | A rule that forces the neighbour below this to have an upConnection
downConnectionRule :: Rule
downConnectionRule = allMustBe upConnection downNeighbour True

-- | A rule that forces the neighbour to the right to have a leftConnection
rightConnectionRule :: Rule
rightConnectionRule = allMustBe leftConnection rightNeighbour True

-- | A rule that forces the neighbour to the left to have a rightConnection
leftConnectionRule :: Rule
leftConnectionRule = allMustBe rightConnection leftNeighbour True

-- | A rule that forces the neighbour above to not have a downConnection
noUpConnectionRule :: Rule
noUpConnectionRule = (<!>) $ nextToAny downConnection upNeighbour

-- | A rule that forces the neighbour below to not have an upConnection
noDownConnectionRule :: Rule
noDownConnectionRule = (<!>) $ nextToAny upConnection downNeighbour

-- | A rule that forces the neighbour to the right to not have a leftConnection
noRightConnectionRule :: Rule
noRightConnectionRule = (<!>) $ nextToAny leftConnection rightNeighbour

-- | A rule that forces the neighbour to the left to not have a rightConnection
noLeftConnectionRule :: Rule
noLeftConnectionRule = (<!>) $ nextToAny rightConnection leftNeighbour

-- | A tile that represents a road crossing, the tile must have a connection in all directions
topCrossTile :: Tile
topCrossTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-cross.png"}) topLevelBlock,
  rules = upConnectionRule <&&> downConnectionRule <&&> rightConnectionRule <&&> leftConnectionRule <&&> posTopRule <&&> weightedRule 0.05 ,
  charRep = '┼'
}

-- | A tile that represents a BLU (Bottom Left Up) road split, the tile must have a connection in all directions except right
topBLUTSplitTile :: Tile
topBLUTSplitTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-BLU-tsplit.png"}) topLevelBlock,
  rules = upConnectionRule <&&> downConnectionRule <&&> leftConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
  charRep = '┤'
}

-- | A tile that represents a URB (Up Right Bottom) road split, the tile must have a connection in all directions except left
topURBTSplitTile :: Tile
topURBTSplitTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-URB-tsplit.png"}) topLevelBlock,
  rules = upConnectionRule <&&> downConnectionRule <&&> rightConnectionRule <&&> noLeftConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
  charRep = '├'
}

-- | A tile that represents a LUR (Left Up Right) road split, the tile must have a connection in all directions except down
topLURTSplitTile :: Tile
topLURTSplitTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-LUR-tsplit.png"}) topLevelBlock,
  rules = leftConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> upConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
  charRep = '┴'
}

-- | A tile that represents a LRB (Left Right Bottom) road split, the tile must have a connection in all directions except up
topLRBTSplitTile :: Tile
topLRBTSplitTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-LRB-tsplit.png"}) topLevelBlock,
  rules = leftConnectionRule <&&> noUpConnectionRule <&&> rightConnectionRule <&&> downConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
  charRep = '┬'
}

-- | A tile that represents a down left corner, the tile must have a connection to the left and below
topLDElbowTile :: Tile
topLDElbowTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-LD-elbow.png"}) topLevelBlock,
  rules = leftConnectionRule <&&> downConnectionRule <&&> noUpConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
  charRep = '┐'
}

-- | A tile that represents a down right corner, the tile must have a connection to the right and below
topDRElbowTile :: Tile
topDRElbowTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-DR-elbow.png"}) topLevelBlock,
  rules = downConnectionRule <&&> rightConnectionRule <&&> noUpConnectionRule <&&> noLeftConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
  charRep = '┌'
}

-- | A tile that represents an up left corner, the tile must have a connection to the left and above
topULElbowTile :: Tile
topULElbowTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-UL-elbow.png"}) topLevelBlock,
  rules = upConnectionRule <&&> leftConnectionRule <&&> noDownConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
  charRep = '┘'
}

-- | A tile that represents an up right corner, the tile must have a connection to the right and above
topURElbowTile :: Tile
topURElbowTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-UR-elbow.png"}) topLevelBlock,
  rules = upConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> noLeftConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
  charRep = '└'
}

-- | A tile that represents a horizontal road, the tile must have a connection to the left and right
topHorizontalPipeTile :: Tile
topHorizontalPipeTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-horizontal-pipe.png"}) topLevelBlock,
  rules = leftConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> noUpConnectionRule <&&> posTopRule <&&> weightedRule 0.6,
  charRep = '─'
}

-- | A tile that represents a vertical road, the tile must have a connection above and below
topVerticalPipeTile :: Tile
topVerticalPipeTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-vertical-pipe.png"}) topLevelBlock,
  rules = upConnectionRule <&&> downConnectionRule <&&> noLeftConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.6,
  charRep = '│'
}

-- | A tile that represents grass, the tile must be placed at a height of 3.
topGrassTile :: Tile
topGrassTile = Tile {
  materials = M.insert PosY (def {texture = Just "textures/top-grass.png"}) topLevelBlock,
  rules = posTopRule,
  charRep = '░'
}

-- | A tile that represents dirt, the tile must be placed at a height of 2 or lower.
dirtTile :: Tile
dirtTile = Tile {
  materials = dirtBlock,
  rules = canExistAt (\(_, y, _) -> y <= 2),
  charRep = 'd'
}

-- | A tile that represents water, the tile above must be a water tile, and the tile must be placed at a height of 1 or higher.
waterTile :: Tile
waterTile = Tile {
  materials = waterBlock,
  rules = allMustBe [waterTile] aboveNeighbour True <&&> canExistAt (\(_, y, _) -> y >= 1) 
      <&&> ((weightedRule 0.05 <&&> (<!>) (nextToAny [waterTile] directNeighbours)) <||> nextToAny [waterTile] directNeighbours),
  charRep = 'w'
}

-- | The tiles that will be used in the wave function collapse algorithm to generate the world
allTiles :: [Tile]
allTiles = [
  topCrossTile, 
  topHorizontalPipeTile, 
  topBLUTSplitTile, 
  topLRBTSplitTile, 
  topLURTSplitTile,
  topURBTSplitTile, 
  topVerticalPipeTile,
  topDRElbowTile,
  topLDElbowTile,
  topULElbowTile,
  topURElbowTile,
  topGrassTile, 
  dirtTile,
  waterTile
  ]
