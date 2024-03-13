module Gen.WaveFuncCollapse where

import Def

import qualified Data.Map as M
import System.Random
import Debug.Trace (trace)

-- TODO
-- 1. Use Shannon entropy for the wave function collapse algorithm

-- | The wave function collapse algorithm is a way to generate a tilemap based on a set of rules.
--   The algorithm works by starting with a completely empty tilemap and then collapsing the wave function
--   of each tile based on the rules and the surrounding tiles. This is done until the entire tilemap is filled.

-- | Weights is a tuple of a float and a list of floats (Total weight, [Weights])
type Weights = (Float, [Float])
type ShannonEntropy = Float
type Env = M.Map Pos ([Tile], Weights, ShannonEntropy)

waveFuncCollapse :: [Tile] -> Size -> IO TileMap
waveFuncCollapse tiles size@((minX, minY, minZ), (maxX, maxY, maxZ)) = do
  let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
  let emptyTileMap = TileMap M.empty
  case createEnv tiles emptyTileMap allPos of
    Nothing -> error "No possible tilemap"
    Just (tileMap, env) -> waveFuncCollapse' tiles tileMap env

createEnv :: [Tile] -> TileMap -> [Pos] -> Maybe (TileMap, Env)
createEnv tiles tileMap = foldr (\pos result -> case result of 
  Nothing -> Nothing
  Just (TileMap newTileMap, newEnv) -> case posToEnv pos of
    Nothing -> Nothing
    Just (Left tile) -> Just (TileMap $ M.insert pos tile newTileMap, newEnv)
    Just (Right env) -> Just (TileMap newTileMap, M.insert pos env newEnv)
    ) (Just (tileMap, M.empty))
  where
    posToEnv :: Pos -> Maybe (Either Tile ([Tile], Weights, ShannonEntropy)) 
    posToEnv pos = 
      let weights = map (resultToFloat . (\(Rule f) -> f tileMap pos) . rules) tiles
          newTiles = map fst $ filter (\(_, weight) -> weight > 0) $ zip tiles weights
      in case newTiles of
        [] -> Nothing
        [tile] -> Just $ Left tile
        _ -> do      
          let totWeight = sum weights
          Just $ Right (newTiles, (totWeight, weights), shannonEntropy (totWeight, weights))

shannonEntropy :: Weights -> ShannonEntropy
shannonEntropy (totWeight, weights) = log totWeight - (h / totWeight)
  where
    h = foldr (\weight prev -> prev + (weight *  log weight)) 0 weights

waveFuncCollapse' :: [Tile] -> TileMap -> Env -> IO TileMap
waveFuncCollapse' tiles tileMap env
  | M.null env = return tileMap
  | otherwise = do
    randomPos <- shannonPos env
    waveFuncCollapseStep tiles randomPos tileMap env >>= \case
      Nothing -> resetWaveFuncCollapse
      Just (newTileMap, newEnv) -> waveFuncCollapse' tiles newTileMap newEnv

resetWaveFuncCollapse :: IO TileMap
resetWaveFuncCollapse = undefined

shannonPos :: Env -> IO Pos
shannonPos env = getRandomElement (snd (M.foldrWithKey minList (0, []) env))
  where
    minList pos (_, _, entropy) list  = case list of
      (0, []) -> (entropy, [pos])
      (minEntropy, ys)
        | entropy < minEntropy -> (entropy, [pos])
        | entropy == minEntropy -> (minEntropy, pos : ys)
        | otherwise -> list

getRandomElement :: [a] -> IO a
getRandomElement xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i)

waveFuncCollapseStep :: [Tile] -> Pos -> TileMap -> Env -> IO (Maybe (TileMap, Env))
waveFuncCollapseStep tiles pos (TileMap tileMap) env = do
  let (tiles, weight, _) = env M.! pos
  tile <- randomTile tiles weight
  case tile of
    Nothing -> return Nothing
    Just tile -> do 
      let newTileMap = M.insert pos tile tileMap
      let newEnv = M.delete pos env
      return $ createEnv tiles (TileMap newTileMap) (M.keys newEnv)

randomTile :: [Tile] -> Weights -> IO (Maybe Tile)
randomTile tiles (totalWeight, tilesWeight) = if totalWeight == 0.0 then return Nothing else do
        randomNum <- randomRIO (0.0, totalWeight)
        return $ weightToTile tiles tilesWeight randomNum

weightToTile :: [Tile] -> [Float] -> Float -> Maybe Tile
weightToTile [] [] _ = Nothing
weightToTile (tile:tiles) (weight:weights) random
  | random < weight = Just tile
  | otherwise = weightToTile tiles weights (random - weight)
weightToTile tiles weights _ = error "WeightToTile: tiles and weights are not the same length"
