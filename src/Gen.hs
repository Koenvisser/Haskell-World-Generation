module Gen where

import Def

import qualified Data.Map as M

-- TODO :) https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
waveFuncCollapse :: [Tile] -> TileMap
waveFuncCollapse _ = undefined

generate :: [Tile] -> TileMap
generate tiles = TileMap $ foldl (\tileMap (x, y, z) -> 
    let validTiles = filter (\tile -> 
            case (\(Rule f) -> f (TileMap tileMap) (x, y, z)) (rules tile) of
            CanPlace b -> b
            ChancePlace c -> c > 0) tiles
    in if not (null validTiles) then M.insert (x, y, z) (head validTiles) tileMap else tileMap
  ) M.empty [(x, y, z) | x <- [0..10], y <- [0..10], z <- [0..10]]
