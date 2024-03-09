module Gen where

import Def

import qualified Data.Map as M
import System.Random
import System.IO.Unsafe
import Debug.Trace (trace)

-- TODO
-- 1. Use Shannon entropy for the wave function collapse algorithm

-- | The wave function collapse algorithm is a way to generate a tilemap based on a set of rules.
--   The algorithm works by starting with a completely empty tilemap and then collapsing the wave function
--   of each tile based on the rules and the surrounding tiles. This is done until the entire tilemap is filled.
waveFuncCollapse :: [Tile] -> Size -> IO TileMap
waveFuncCollapse tiles size@((minX, minY, minZ), (maxX, maxY, maxZ)) = do
  let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
  let superPos = superPosition tiles ((minX, minY, minZ), (maxX, maxY, maxZ))
  let emptyTileMap = TileMap M.empty
  waveFuncCollapse' allPos emptyTileMap superPos

takeAndRemove :: Int -> [a] -> (a, [a])
takeAndRemove n xs = (xs !! n, take n xs ++ drop (n+1) xs)

waveFuncCollapse' :: [Pos] -> TileMap -> M.Map Pos [Tile] -> IO TileMap
waveFuncCollapse' [] tileMap _ = return tileMap
waveFuncCollapse' allPos (TileMap tileMap) tilesMap = do
  randomNum <- randomRIO (0, length allPos - 1)
  let (pos, newAllPos) = takeAndRemove randomNum allPos
  waveFuncCollapseStep pos (TileMap tileMap) tilesMap >>= \case
    Nothing -> return (TileMap tileMap) -- Reset wave function collapse?
    Just (newTileMap, newTilesMap) -> waveFuncCollapse' newAllPos newTileMap newTilesMap 

superPosition :: [Tile] -> Size -> M.Map Pos [Tile]
superPosition tiles ((minX, minY, minZ), (maxX, maxY, maxZ)) = 
        M.fromList [(pos, tiles) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ], let pos = (x, y, z)]

waveFuncCollapseStep :: Pos -> TileMap -> M.Map Pos [Tile] -> IO (Maybe (TileMap, M.Map Pos [Tile]))
waveFuncCollapseStep pos (TileMap tileMap) tilesMap = do
  tile <- randomTile pos (tilesMap M.! pos) (TileMap tileMap)
  case tile of
    Nothing -> return Nothing
    Just tile -> return $ do
      (newTilesMap, newTileMap) <- M.foldrWithKey (\p tiles maps -> case maps of
            Nothing -> Nothing
            Just (newTilesMap, newTileMap) -> 
              if p == pos then Just (newTilesMap, M.insert p tile newTileMap) else do
                    let newTiles = filter ((\(Rule f) -> resultToBool (f (TileMap tileMap) pos)) . rules) tiles
                    case newTiles of
                      [] -> Nothing
                      [tile] -> Just (newTilesMap, M.insert p tile newTileMap)
                      _ -> Just (M.insert p newTiles newTilesMap, newTileMap)
                ) (Just (M.empty, tileMap)) tilesMap
      return (TileMap newTileMap, newTilesMap)

randomTile :: Pos -> [Tile] -> TileMap -> IO (Maybe Tile)
randomTile pos tiles tileMap = if totalWeight == 0.0 then return Nothing else do
        randomNum <- randomRIO (0.0, totalWeight)
        return $ weightToTile tiles tilesWeight randomNum
        where
        rulesResults = map ((\(Rule f) -> f tileMap pos) . rules) tiles
        tilesWeight = map resultToFloat rulesResults
        totalWeight :: Float
        totalWeight = sum tilesWeight

weightToTile :: [Tile] -> [Float] -> Float -> Maybe Tile
weightToTile [] [] _ = Nothing
weightToTile (tile:tiles) (weight:weights) random
  | random < weight = Just tile
  | otherwise = weightToTile tiles weights (random - weight)
weightToTile tiles weights _ = error "WeightToTile: tiles and weights are not the same length"


generate :: [Tile] -> TileMap
generate tiles = TileMap $ foldl (\tileMap (x, y, z) -> 
    let validTiles = filter (\tile -> 
            case (\(Rule f) -> f (TileMap tileMap) (x, y, z)) (rules tile) of
            CanPlace b -> b
            ChancePlace c -> isHigherThanRandom c) tiles
    in if not (null validTiles) then M.insert (x, y, z) (head validTiles) tileMap else tileMap
  ) M.empty [(x, y, z) | x <- [0..10], y <- [0..10], z <- [0..10]]


isHigherThanRandom :: Float -> Bool
isHigherThanRandom n = unsafePerformIO $ do
    randomNum <- randomRIO (0.0, 1.0) -- generates a random number between 1 and 100
    return  (n > randomNum)
