module RoadExample where

import Def
import Utils
import Data.Default (def)
import qualified Data.Map as M

-- dirt on all sides

-- Top level blocks grass-dirt side, dirt bottom, grass/road top

dirtMaterial :: Material
dirtMaterial = def {texture = Just "textures/side-dirt.png"}

grassDirtMaterialSide :: Material
grassDirtMaterialSide = def {texture = Just "textures/side-dirt-grass.png"}


topLevelBlock :: M.Map Side Material
topLevelBlock = M.fromList [
    (NegY, dirtMaterial),
    (NegX, grassDirtMaterialSide),
    (PosX, grassDirtMaterialSide),
    (NegZ, grassDirtMaterialSide),
    (PosZ, grassDirtMaterialSide)
    ]

dirtBlock :: M.Map Side Material
dirtBlock = M.fromList [
    (PosZ, dirtMaterial),
    (NegZ, dirtMaterial),
    (NegX, dirtMaterial),
    (NegY, dirtMaterial),
    (PosX, dirtMaterial),
    (PosY, dirtMaterial)
    ]

waterMaterial :: Material
waterMaterial = def {diffuseColor = (0.0, 0.0, 1.0), transparency = 0.5}

waterBlock :: M.Map Side Material
waterBlock = M.fromList [
    (NegY, waterMaterial),
    (PosY, waterMaterial),
    (NegX, waterMaterial),
    (PosX, waterMaterial),
    (NegZ, waterMaterial),
    (PosZ, waterMaterial)
    ]

posTopRule :: Rule
posTopRule = canExistAt (\(_, y, _) -> y == 3)

verticalNeighbour :: Shape
verticalNeighbour = listToShape [(0, 0, -1), (0, 0, 1)]

horizontalNeighbour :: Shape
horizontalNeighbour = listToShape [(-1, 0, 0), (1, 0, 0)]

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

upConnectionRule :: Rule
upConnectionRule = allMustBe downConnection upNeighbour

downConnectionRule :: Rule
downConnectionRule = allMustBe upConnection downNeighbour

rightConnectionRule :: Rule
rightConnectionRule = allMustBe leftConnection rightNeighbour

leftConnectionRule :: Rule
leftConnectionRule = allMustBe rightConnection leftNeighbour 

noUpConnectionRule :: Rule
noUpConnectionRule = (<!>) $ nextToAny downConnection upNeighbour

noDownConnectionRule :: Rule
noDownConnectionRule = (<!>) $ nextToAny upConnection downNeighbour

noRightConnectionRule :: Rule
noRightConnectionRule = (<!>) $ nextToAny leftConnection rightNeighbour

noLeftConnectionRule :: Rule
noLeftConnectionRule = (<!>) $ nextToAny rightConnection leftNeighbour

topCrossTile :: Tile
topCrossTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-cross.png"}) topLevelBlock,
    rules = upConnectionRule <&&> downConnectionRule <&&> rightConnectionRule <&&> leftConnectionRule <&&> posTopRule <&&> weightedRule 0.05 ,
    charRep = '┼'
}

-- | Top left up top split
topBLUTSplitTile :: Tile
topBLUTSplitTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-BLU-tsplit.png"}) topLevelBlock,
    rules = upConnectionRule <&&> downConnectionRule <&&> leftConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
    charRep = '┤'
}

topURBTSplitTile :: Tile
topURBTSplitTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-URB-tsplit.png"}) topLevelBlock,
    rules = upConnectionRule <&&> downConnectionRule <&&> rightConnectionRule <&&> noLeftConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
    charRep = '├'
}

topLURTSplitTile :: Tile
topLURTSplitTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-LUR-tsplit.png"}) topLevelBlock,
    rules = leftConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> upConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
    charRep = '┴'
}

topLRBTSplitTile :: Tile
topLRBTSplitTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-LRB-tsplit.png"}) topLevelBlock,
    rules = leftConnectionRule <&&> noUpConnectionRule <&&> rightConnectionRule <&&> downConnectionRule <&&> posTopRule <&&> weightedRule 0.1,
    charRep = '┬'
}

topLDElbowTile :: Tile
topLDElbowTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-LD-elbow.png"}) topLevelBlock,
    rules = leftConnectionRule <&&> downConnectionRule <&&> noUpConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
    charRep = '┐'
}

topDRElbowTile :: Tile
topDRElbowTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-DR-elbow.png"}) topLevelBlock,
    rules = downConnectionRule <&&> rightConnectionRule <&&> noUpConnectionRule <&&> noLeftConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
    charRep = '┌'
}

topULElbowTile :: Tile
topULElbowTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-UL-elbow.png"}) topLevelBlock,
    rules = upConnectionRule <&&> leftConnectionRule <&&> noDownConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
    charRep = '┘'
}

topURElbowTile :: Tile
topURElbowTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-UR-elbow.png"}) topLevelBlock,
    rules = upConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> noLeftConnectionRule <&&> posTopRule <&&> weightedRule 0.2,
    charRep = '└'
}

topHorizontalPipeTile :: Tile
topHorizontalPipeTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-horizontal-pipe.png"}) topLevelBlock,
    rules = leftConnectionRule <&&> rightConnectionRule <&&> noDownConnectionRule <&&> noUpConnectionRule <&&> posTopRule <&&> weightedRule 0.6,
    charRep = '─'
}

topVerticalPipeTile :: Tile
topVerticalPipeTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-vertical-pipe.png"}) topLevelBlock,
    rules = upConnectionRule <&&> downConnectionRule <&&> noLeftConnectionRule <&&> noRightConnectionRule <&&> posTopRule <&&> weightedRule 0.6,
    charRep = '│'
}


topGrassTile :: Tile
topGrassTile = Tile {
    materials = M.insert PosY (def {texture = Just "textures/top-grass.png"}) topLevelBlock,
    rules = posTopRule,
    charRep = '░'
}

dirtTile :: Tile
dirtTile = Tile {
    materials = dirtBlock,
    rules = canExistAt (\(_, y, _) -> y <= 2),
    charRep = 'd'
}

waterTile :: Tile
waterTile = Tile {
    materials = waterBlock,
    rules = allMustBe [waterTile] aboveNeighbour <&&> canExistAt (\(_, y, _) -> y >= 1) 
        <&&> ((weightedRule 0.05 <&&> (<!>) (nextToAny [waterTile] directNeighbours)) <||> nextToAny [waterTile] directNeighbours),
    charRep = 'w'
}

allTiles :: [Tile]
allTiles = [topCrossTile, 
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
            waterTile]