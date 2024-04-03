-- | The wave function collapse algorithm is a way to generate a tilemap based on a set of rules.
--   The algorithm works by starting with a completely empty tilemap and then collapsing the wave function
--   of each tile based on the rules and the surrounding tiles. This is done until the entire tilemap is filled.
module Gen.WaveFuncCollapse where

import Def

import qualified Data.Map as M
import System.Random
import Debug.Trace (trace)
import Data.List (delete)

type WFCMap = M.Map Pos [Pos]
-- | Weights is a tuple of a float and a list of floats (Total weight, [Weights])
type Weights = (Float, [Float])
type ShannonEntropy = Float
type Env = M.Map Pos ([Tile], Weights, ShannonEntropy)
type History = [HistoryUnit]
data HistoryUnit = HistoryUnit {
  tileMap :: TileMap,
  env :: Env,
  pos :: Pos,
  tile :: Tile
} deriving (Show)

-- | Initialize the wave function collapse algorithm with a list of tiles and a size for the world.
--   The algorithm will then generate a tilemap and an environment using the `createEnv` function.
--   If no Environment can be created with the current Tile set, the function throw an error.
--   Afterwards it will invoke `waveFuncCollapse'` which will iteratiively collapse the wave function. 
waveFuncCollapse :: [Tile] -> Size -> IO TileMap
waveFuncCollapse tiles size@((minX, minY, minZ), (maxX, maxY, maxZ)) = do
  let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
  let emptyTileMap = TileMap M.empty
  case createEnv tiles emptyTileMap allPos of
    Nothing -> error "No possible tilemap"
    Just (tileMap, env) -> waveFuncCollapse' tileMap env []

-- | Create the tileMap and environment for the wave function collapse algorithm
--   It applies the rule to each position and creates the environment based on the result.
--   If an environment only has one possible tile it will be added to the tileMap, 
--   and removed from the environment. If an environment has no possible tiles, the function will return Nothing.
createEnv :: [Tile] -> TileMap -> [Pos] -> Maybe (TileMap, Env)
createEnv tiles tileMap = foldr (\pos result -> case result of 
  Nothing -> Nothing
  Just (TileMap newTileMap, newEnv) -> case posToEnv pos tiles (TileMap newTileMap) of
    Nothing -> Nothing
    Just (Left tile) -> Just (TileMap $ M.insert pos tile newTileMap, newEnv)
    Just (Right env) -> Just (TileMap newTileMap, M.insert pos env newEnv)
    ) (Just (tileMap, M.empty))

updateEnv :: TileMap -> Env -> Maybe (TileMap, Env)
updateEnv tileMap env = foldr (\pos result -> case result of 
  Nothing -> Nothing
  Just (TileMap newTileMap, newEnv) -> let (tiles, _, _) = env M.! pos in
    case posToEnv pos tiles (TileMap newTileMap) of
      Nothing -> Nothing
      Just (Left tile) -> Just (TileMap $ M.insert pos tile newTileMap, newEnv)
      Just (Right env) -> Just (TileMap newTileMap, M.insert pos env newEnv)
    ) (Just (tileMap, M.empty)) (M.keys env)

posToEnv :: Pos -> [Tile] -> TileMap -> Maybe (Either Tile ([Tile], Weights, ShannonEntropy)) 
posToEnv pos tiles (TileMap tileMap) = 
  let weights = map (resultToFloat . (\(Rule rule) -> rule (TileMap tileMap) pos) . rules) tiles
      -- Only keep the tiles that have a weight greater than 0 and still satisfy the rules
      (newTiles, newWeights) = unzip $ filter (\(tile, weight) -> weight > 0 && all (\(pos', tile') ->
        -- Check if the rule still holds for any tile in the tileMap if the new tile is placed at the position
        ((0<) . resultToFloat . (\(Rule rule) -> rule (TileMap (M.insert pos tile tileMap)) pos') . rules) tile') 
          (M.toList tileMap)) $ zip tiles weights
  in case newTiles of
    [] -> Nothing
    [tile] -> Just $ Left tile
    _ -> do
      let totWeight = sum newWeights
      Just $ Right (newTiles, (totWeight, newWeights), shannonEntropy (totWeight, newWeights))

-- | Calculate the shannon entropy of a set of tiles. We use type `Weights` instead of a list of tiles
--   to avoid reevaluating the rules of the tiles and recalculating the total weight of these tiles.
shannonEntropy :: Weights -> ShannonEntropy
shannonEntropy (totWeight, weights) = log totWeight - (h / totWeight)
  where
    h = foldr (\weight prev -> prev + (weight *  log weight)) 0 weights

-- | The helper function of the `waveFuncCollape` function. It will iteratively call the `waveFuncCollapseStep`
--   function until there are no more available positions in the environment. This function is called with the 
--   position where the wave function should collapse. This position is generated using the `shannonPos` 
--   function. If the algorithm gets stuck, it will call the `resetWaveFuncCollapse` function. 
waveFuncCollapse' :: TileMap -> Env -> History -> IO TileMap
waveFuncCollapse' tileMap env history
  | M.null env = return tileMap
  | otherwise = do
    randomPos <- shannonPos env
    (result, newHistory) <- waveFuncCollapseStep randomPos tileMap env history
    case result of
      Nothing -> resetWaveFuncCollapse newHistory
      Just (newTileMap, newEnv) -> waveFuncCollapse' newTileMap newEnv newHistory

-- | Reset the wave function collapse algorithm. This is done when the algorithm gets stuck and can't continue.
--   Not implemented yet. Optimally the algorithm should be able to backtrack to a previously solvable state.
resetWaveFuncCollapse :: History -> IO TileMap
resetWaveFuncCollapse [] = error "No possible tilemaps with the current rules"
resetWaveFuncCollapse ((HistoryUnit tileMap env pos tile):history) = do
  let newEnv = M.adjust (\(tiles, weights, _) -> let (newTiles, newWeights) = deleteTile tile (tiles, weights) in (newTiles, newWeights, shannonEntropy newWeights)) pos env
  let (newTiles, _, _) = newEnv M.! pos
  if null newTiles then resetWaveFuncCollapse history else waveFuncCollapse' tileMap newEnv history
  where
    deleteTile :: Tile -> ([Tile], Weights) -> ([Tile], Weights)
    deleteTile tile (x:xs, (totalWeight, y:ys)) 
      | x == tile = (xs, (totalWeight - y, ys))
      | otherwise = (\(newTiles', (totalWeight', weights)) -> (x:newTiles', (totalWeight' + y, y:weights))) $ deleteTile tile (xs, (totalWeight - y, ys)) 


-- | Get the position with the lowest shannon entropy. If there are multiple positions with the same entropy,
--   the function will return a random position from the list of positions.
shannonPos :: Env -> IO Pos
shannonPos env = getRandomElement (snd (M.foldrWithKey minList (0, []) env))
  where
    minList pos (_, _, entropy) list  = case list of
      (0, []) -> (entropy, [pos])
      (minEntropy, ys)
        | entropy < minEntropy -> (entropy, [pos])
        | entropy == minEntropy -> (minEntropy, pos : ys)
        | otherwise -> list

-- | Get a random element from a list
getRandomElement :: [a] -> IO a
getRandomElement xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i)

-- | Collapse the wave function of a single position. Selects a random tile from the environment and
--  adds it to the tileMap. If the environment has no possible tiles, the function will return Nothing.
--  After the tile has been added to the tileMap, the position will be removed from the environment,
--  and the environment will be updated with the new possible tiles.
waveFuncCollapseStep :: Pos -> TileMap -> Env -> History -> IO (Maybe (TileMap, Env), History)
waveFuncCollapseStep pos (TileMap tileMap) env history = do
  let (newTiles, weight, _) = env M.! pos
  tile <- randomTile newTiles weight
  case tile of
    Nothing -> return (Nothing, history)
    Just tile -> do 
      let newTileMap = M.insert pos tile tileMap
      let newHistory = HistoryUnit (TileMap tileMap) env pos tile : history
      let newEnv = M.delete pos env
      return (updateEnv (TileMap newTileMap) newEnv >>= (\(newTileMap, newEnv) -> Just (newTileMap, newEnv)), newHistory)

-- | Select a random tile from a list of tiles based on their weights.
--   If the total weight is 0, the function will return Nothing.
randomTile :: [Tile] -> Weights -> IO (Maybe Tile)
randomTile tiles (totalWeight, tilesWeight) = if totalWeight == 0.0 then return Nothing else do
        randomNum <- randomRIO (0.0, totalWeight)
        return $ weightToTile tiles tilesWeight randomNum

-- | Convert a list of tiles and their weights to a single tile based on a random number.
--  The function will return Nothing if list of tiles and weights are empty or the random number is 
--  greater than the total weight. The length of the tiles and weights must be the same.
weightToTile :: [Tile] -> [Float] -> Float -> Maybe Tile
weightToTile [] [] _ = Nothing
weightToTile (tile:tiles) (weight:weights) randomNum
  | randomNum < weight = Just tile
  | otherwise = weightToTile tiles weights (randomNum - weight)
weightToTile _ _ _ = error "WeightToTile: tiles and weights are not the same length"
