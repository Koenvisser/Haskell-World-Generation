-- | A module that defines a set of tiles that can be used to generate a road-like structure.
--   The roads must be connected to each other, and the road can have a turn, a split, a cross, a straight line, or an elbow.
--   The world can also contain dirt, grass and water.
module WorldGen.Examples.Roads where

import WorldGen.Def
import WorldGen.Utils
import Paths_Haskell_World_Generation (getDataFileName)

import Data.Default (def)
import qualified Data.Map as M

-- | The material that will be used to represent dirt, uses a dirt texture
dirtMaterial :: IO Material
dirtMaterial = do 
  tex <- getDataFileName "textures/side-dirt.png"
  return $ def {texture = Just tex}

-- | The material that will be used to represent grass, uses a grass texture
grassDirtMaterialSide :: IO Material
grassDirtMaterialSide = do 
  tex <- getDataFileName "textures/side-dirt-grass.png"
  return $ def {texture = Just tex}

-- | The material map  that will be used to represent a top level block, the PosY still has to be defined
topLevelBlock :: IO (M.Map Side Material)
topLevelBlock = do 
  dirt <- dirtMaterial
  grassDirt <- grassDirtMaterialSide
  return $ M.fromList [
    (NegY, dirt),
    (NegX, grassDirt),
    (PosX, grassDirt),
    (NegZ, grassDirt),
    (PosZ, grassDirt)
    ]

-- | The material map that will be used to represent a dirt block, the PosY is already defined as dirtMaterial
dirtBlock :: IO (M.Map Side Material)
dirtBlock = do
  createMaterialMapForAllSides <$> dirtMaterial

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
leftConnection :: IO [Tile]
leftConnection = sequence [
  topCrossTile, 
  topBLUTSplitTile, 
  topHorizontalPipeTile,
  topLURTSplitTile,
  topLRBTSplitTile,
  topLDElbowTile,
  topULElbowTile
  ]

-- | A list of all tiles that have a right connection
rightConnection :: IO [Tile]
rightConnection = sequence [
  topCrossTile, 
  topHorizontalPipeTile, 
  topURBTSplitTile,
  topLRBTSplitTile,
  topLURTSplitTile,
  topDRElbowTile,
  topURElbowTile
  ]

-- | A list of all tiles that have an up connection
upConnection :: IO [Tile]
upConnection = sequence [
  topCrossTile, 
  topBLUTSplitTile,
  topLURTSplitTile,
  topURBTSplitTile,
  topVerticalPipeTile,
  topURElbowTile,
  topULElbowTile
  ]

-- | A list of all tiles that have a down connection
downConnection :: IO [Tile]
downConnection = sequence [
  topCrossTile, 
  topBLUTSplitTile,
  topURBTSplitTile,
  topLRBTSplitTile, 
  topVerticalPipeTile,
  topDRElbowTile,
  topLDElbowTile
  ]

-- | A rule that forces the neighbour above this to have a downConnection
upConnectionRule :: IO Rule
upConnectionRule = do 
  down <- downConnection
  return $ allMustBe down upNeighbour True

-- | A rule that forces the neighbour below this to have an upConnection
downConnectionRule :: IO Rule
downConnectionRule = do 
  up <- upConnection
  return $ allMustBe up downNeighbour True

-- | A rule that forces the neighbour to the right to have a leftConnection
rightConnectionRule :: IO Rule
rightConnectionRule = do 
  left <- leftConnection
  return $ allMustBe left rightNeighbour True

-- | A rule that forces the neighbour to the left to have a rightConnection
leftConnectionRule :: IO Rule
leftConnectionRule = do 
  right <- rightConnection
  return $ allMustBe right leftNeighbour True

-- | A rule that forces the neighbour above to not have a downConnection
noUpConnectionRule :: IO Rule
noUpConnectionRule = do 
  down <- downConnection
  return $ (<!>) $ nextToAny down upNeighbour

-- | A rule that forces the neighbour below to not have an upConnection
noDownConnectionRule :: IO Rule
noDownConnectionRule = do 
  up <- upConnection
  return $ (<!>) $ nextToAny up downNeighbour

-- | A rule that forces the neighbour to the right to not have a leftConnection
noRightConnectionRule :: IO Rule
noRightConnectionRule = do 
  left <- leftConnection
  return $ (<!>) $ nextToAny left rightNeighbour

-- | A rule that forces the neighbour to the left to not have a rightConnection
noLeftConnectionRule :: IO Rule
noLeftConnectionRule = do 
  right <- rightConnection
  return $ (<!>) $ nextToAny right leftNeighbour

-- | A tile that represents a road crossing, the tile must have a connection in all directions
topCrossTile :: IO Tile
topCrossTile = do 
  tex <- getDataFileName "textures/top-cross.png"
  top <- topLevelBlock
  rule <- upConnectionRule <&&> downConnectionRule <&&> rightConnectionRule <&&> leftConnectionRule <&&> return (posTopRule <&&> weightedRule 0.05)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule ,
    charRep = '┼'
  }

-- | A tile that represents a BLU (Bottom Left Up) road split, the tile must have a connection in all directions except right
topBLUTSplitTile :: IO Tile
topBLUTSplitTile = do 
  tex <- getDataFileName "textures/top-BLU-tsplit.png"
  top <- topLevelBlock
  rule <- leftConnectionRule <&&> upConnectionRule <&&> downConnectionRule <&&> noRightConnectionRule <&&> return (posTopRule <&&> weightedRule 0.1)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '┤'
  }

-- | A tile that represents a URB (Up Right Bottom) road split, the tile must have a connection in all directions except left
topURBTSplitTile :: IO Tile
topURBTSplitTile = do 
  tex <- getDataFileName "textures/top-URB-tsplit.png"
  top <- topLevelBlock
  rule <- upConnectionRule <&&> rightConnectionRule <&&> downConnectionRule <&&> noLeftConnectionRule <&&> return (posTopRule <&&> weightedRule 0.1)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '├'
  }

-- | A tile that represents a LUR (Left Up Right) road split, the tile must have a connection in all directions except down
topLURTSplitTile :: IO Tile
topLURTSplitTile = do 
  tex <- getDataFileName "textures/top-LUR-tsplit.png"
  top <- topLevelBlock
  rule <- leftConnectionRule <&&> upConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> return (posTopRule <&&> weightedRule 0.1)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '┴'
  }

-- | A tile that represents a LRB (Left Right Bottom) road split, the tile must have a connection in all directions except up
topLRBTSplitTile :: IO Tile
topLRBTSplitTile = do 
  tex <- getDataFileName "textures/top-LRB-tsplit.png"
  top <- topLevelBlock
  rule <- leftConnectionRule <&&> rightConnectionRule <&&> downConnectionRule <&&> noUpConnectionRule <&&> return (posTopRule <&&> weightedRule 0.1)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '┬'
  }

-- | A tile that represents a down left corner, the tile must have a connection to the left and below
topLDElbowTile :: IO Tile
topLDElbowTile = do 
  tex <- getDataFileName "textures/top-LD-elbow.png"
  top <- topLevelBlock
  rule <- downConnectionRule <&&> leftConnectionRule <&&> noUpConnectionRule <&&> noRightConnectionRule <&&> return (posTopRule <&&> weightedRule 0.2)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '┐'
  }

-- | A tile that represents a down right corner, the tile must have a connection to the right and below
topDRElbowTile :: IO Tile
topDRElbowTile = do 
  tex <- getDataFileName "textures/top-DR-elbow.png"
  top <- topLevelBlock
  rule <- downConnectionRule <&&> rightConnectionRule <&&> noUpConnectionRule <&&> noLeftConnectionRule <&&> return (posTopRule <&&> weightedRule 0.2)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '┌'
  }

-- | A tile that represents an up left corner, the tile must have a connection to the left and above
topULElbowTile :: IO Tile
topULElbowTile = do 
  tex <- getDataFileName "textures/top-UL-elbow.png"
  top <- topLevelBlock
  rule <- upConnectionRule <&&> leftConnectionRule <&&> noDownConnectionRule <&&> noRightConnectionRule <&&> return (posTopRule <&&> weightedRule 0.2)
  return $ Tile {
      materials = M.insert PosY (def {texture = Just tex}) top,
      rules = rule,
      charRep = '┘'
    }

-- | A tile that represents an up right corner, the tile must have a connection to the right and above
topURElbowTile :: IO Tile
topURElbowTile = do 
  tex <- getDataFileName "textures/top-UR-elbow.png"
  top <- topLevelBlock
  rule <- upConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> noLeftConnectionRule <&&> return (posTopRule <&&> weightedRule 0.2)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '└'
  }

-- | A tile that represents a horizontal road, the tile must have a connection to the left and right
topHorizontalPipeTile :: IO Tile
topHorizontalPipeTile = do 
  tex <- getDataFileName "textures/top-horizontal-pipe.png"
  top <- topLevelBlock
  rule <- leftConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> noUpConnectionRule <&&> return (posTopRule <&&> weightedRule 0.6)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '─'
  }

-- | A tile that represents a vertical road, the tile must have a connection above and below
topVerticalPipeTile :: IO Tile
topVerticalPipeTile = do 
  tex <- getDataFileName "textures/top-vertical-pipe.png"
  top <- topLevelBlock
  rule <- upConnectionRule <&&> downConnectionRule <&&> noLeftConnectionRule <&&> noRightConnectionRule <&&> return (posTopRule <&&> weightedRule 0.6)
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = rule,
    charRep = '│'
  }

-- | A tile that represents grass, the tile must be placed at a height of 3.
topGrassTile :: IO Tile
topGrassTile = do 
  tex <- getDataFileName "textures/top-grass.png"
  top <- topLevelBlock
  return $ Tile {
    materials = M.insert PosY (def {texture = Just tex}) top,
    rules = posTopRule,
    charRep = '░'
  }

-- | A tile that represents dirt, the tile must be placed at a height of 2 or lower.
dirtTile :: IO Tile
dirtTile = do 
  dirt <- dirtBlock
  return $ Tile {
    materials = dirt,
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
allTiles :: IO [Tile]
allTiles = sequence [
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
  return waterTile
  ]
