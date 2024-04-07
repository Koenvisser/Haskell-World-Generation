-- | The wave function collapse algorithm is a way to generate a tilemap based on a set of rules.
--   The algorithm works by starting with a completely empty tilemap and then collapsing the wave function
--   of each tile based on the rules and the surrounding tiles. This is done until the entire tilemap is filled.
module Gen.WaveFuncCollapse (waveFuncCollapse, Error) where

import Internal.Def (getVal, getPos)
import Def

import qualified Data.Map as M
import System.Random

-- | `Dependencies` is a map of positions to a list of positions. It is used to keep track of which positions
--   are dependent on which other positions. An example of this is a tile which rules are dependent on its neighbours,
---  the neighbours of this tile are then dependent on the tile itself, so the position of the tile is added to the
--   list of dependencies of the neighbours.
type Dependencies = M.Map Pos [Pos]
-- | Weights is a tuple of a float and a list of floats (Total weight, [Weights])
type Weights = (Float, [Float])
type ShannonEntropy = Float
-- | The `Env` is a map of positions to a tuple of a list of tiles, weights and shannon entropy.
--   This is used to keep track of the possible tiles for each position and their weights.
type Env = M.Map Pos ([Tile], Weights, ShannonEntropy)
-- | The `History` is a list of `HistoryUnit` which is used to keep track of the history of the wave function collapse.
type History = [HistoryUnit]
-- | The `HistoryUnit` is a record of the tilemap, environment, dependencies, position and tile of a single step in the wave function collapse.
data HistoryUnit = HistoryUnit {
  prevTileMap :: TileMap,
  prevEnv :: Env,
  prevDependencies :: Dependencies,
  placedPos :: Pos,
  placedTile :: Tile
} deriving (Show)

type Error = String

-- | Run the wave function collapse algorithm on a list of tiles and a size for the world.
--   The algorithm will return a tilemap that satisfies the rules of the tiles.
--   If no possible tilemap can be generated, the function will return an error. 
waveFuncCollapse :: [Tile] -> Size -> IO (Either Error TileMap)
waveFuncCollapse tiles ((minX, minY, minZ), (maxX, maxY, maxZ)) = do
  let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
  let emptyTileMap = TileMap M.empty
  case createEnv tiles emptyTileMap allPos of
    Nothing -> return $ Left "No possible tilemap can be generated"
    Just (tileMap, env, dependencies) -> waveFuncCollapse' tileMap env dependencies []

-- | Create the tileMap and environment for the wave function collapse algorithm
--   It applies the rule to each position and creates the environment based on the result.
--   If an environment only has one possible tile it will be added to the tileMap, 
--   and removed from the environment. If an environment has no possible tiles, the function will return Nothing.
createEnv :: [Tile] -> TileMap -> [Pos] -> Maybe (TileMap, Env, Dependencies)
createEnv tiles tileMap = foldr (\pos result -> case result of 
  Nothing -> Nothing
  Just (TileMap newTileMap, newEnv, newDependencies) -> case posToEnv pos tiles (TileMap newTileMap) newDependencies of
    Nothing -> Nothing
    Just (Left tile) -> Just (TileMap $ M.insert pos tile newTileMap, newEnv, updateDependencies pos tile (TileMap newTileMap) newDependencies)
    Just (Right env) -> Just (TileMap newTileMap, M.insert pos env newEnv, newDependencies)
    ) (Just (tileMap, M.empty, M.empty))

-- | Update the environment with the new possible tiles and their weights. 
--   It applies the rule to each possible tile and removes the tile from the environment if it doesn't satisfy the rules.
--   If the environment has only one possible tile it will be added to the tileMap and removed from the environment. 
--   If the environment has no possible tiles, the function will return Nothing.
--   Also updates the dependencies of the positions based on the new tile.
updateEnv :: TileMap -> Env -> Dependencies -> Maybe (TileMap, Env, Dependencies)
updateEnv tileMap env dependencies = foldr (\pos result -> case result of 
  Nothing -> Nothing
  Just (TileMap newTileMap, newEnv, newDependencies) -> let (tiles, _, _) = env M.! pos in
    case posToEnv pos tiles (TileMap newTileMap) newDependencies of
      Nothing -> Nothing
      Just (Left tile) -> Just (TileMap $ M.insert pos tile newTileMap, newEnv, updateDependencies pos tile (TileMap newTileMap) newDependencies)
      Just (Right newPosEnv) -> Just (TileMap newTileMap, M.insert pos newPosEnv newEnv, newDependencies)
    ) (Just (tileMap, M.empty, dependencies)) (M.keys env)

-- | Calculates the new environment for a position. It applies the rules to the position and returns the possible tiles
--   and their weights. If the environment has only one possible tile it will return a `Left Tile`, otherwise it will return
--   a `Right ([Tile], Weights, ShannonEntropy)`. If the environment has no possible tiles, the function will return Nothing.
posToEnv :: Pos -> [Tile] -> TileMap -> Dependencies -> Maybe (Either Tile ([Tile], Weights, ShannonEntropy)) 
posToEnv pos tiles (TileMap tileMap) dependencies = 
  let weights = map (resultToFloat . getVal . (\(Rule rule) -> rule (TileMap tileMap) pos) . rules) tiles
      -- Only keep the tiles that have a weight greater than 0 and still satisfy the rules
      (newTiles, newWeights) = unzip $ filter (\(tile, weight) -> weight > 0 && 
        -- Check if the depencies of the tile still satisfy the rules if the tile is placed at the position
        case M.lookup pos dependencies of 
          Just deps -> all (\pos' -> 
            maybe True ((0<) . resultToFloat . getVal . (\(Rule rule) -> rule (TileMap (M.insert pos tile tileMap)) pos') . rules) (M.lookup pos' tileMap) 
              ) deps
          Nothing -> True) $ zip tiles weights
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
waveFuncCollapse' :: TileMap -> Env -> Dependencies -> History -> IO (Either Error TileMap)
waveFuncCollapse' tileMap env dependencies history
  | M.null env = return $ Right tileMap
  | otherwise = do
    randomPos <- shannonPos env
    (result, newHistory) <- waveFuncCollapseStep randomPos tileMap env dependencies history
    case result of
      Nothing -> resetWaveFuncCollapse newHistory
      Just (newTileMap, newEnv, newDependencies) -> waveFuncCollapse' newTileMap newEnv newDependencies newHistory

-- | Reset the wave function collapse algorithm. This is done when the algorithm gets stuck and can't continue.
--   Not implemented yet. Optimally the algorithm should be able to backtrack to a previously solvable state.
resetWaveFuncCollapse :: History -> IO (Either Error TileMap)
resetWaveFuncCollapse [] = return $ Left "No possible tilemap can be generated"
resetWaveFuncCollapse ((HistoryUnit tileMap env dependencies pos tile):history) = do
  let newEnv = M.adjust (\(tiles, weights, _) -> let (newTiles, newWeights) = deleteTile tile (tiles, weights) in (newTiles, newWeights, shannonEntropy newWeights)) pos env
  let (newTiles, _, _) = newEnv M.! pos
  if null newTiles then resetWaveFuncCollapse history else waveFuncCollapse' tileMap newEnv dependencies history

-- | Delete a tile from a list of tiles and their weights. The function will return a new list of tiles and weights
--   where the tile has been removed. If the tile is not in the list, the function will throw an error.
deleteTile :: Tile -> ([Tile], Weights) -> ([Tile], Weights)
deleteTile tile (x:xs, (totalWeight, y:ys)) 
  | x == tile = (xs, (totalWeight - y, ys))
  | otherwise = (\(newTiles', (totalWeight', weights)) -> (x:newTiles', (totalWeight' + y, y:weights))) $ deleteTile tile (xs, (totalWeight - y, ys)) 
deleteTile _ _ = error "Tile not in list"

-- | Get the position with the lowest shannon entropy. If there are multiple positions with the same entropy,
--   the function will return a random position from the list of positions.
shannonPos :: Env -> IO Pos
shannonPos env = getRandomElement (snd (M.foldrWithKey minList (0, []) env))
  where
    minList :: Pos -> ([Tile], Weights, ShannonEntropy) -> (ShannonEntropy, [Pos]) -> (ShannonEntropy, [Pos]) 
    minList pos (_, _, entropy) list = case list of
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

-- | Update the dependencies of a position, where a `Tile` has been placed. The function will remove the position
--   from the dependencies and insert the position of the tile at the position of the dependencies.
updateDependencies :: Pos -> Tile -> TileMap -> Dependencies -> Dependencies
updateDependencies pos tile tileMap dependencies = let 
  (Rule rule) = rules tile
  deps = getPos $ rule tileMap pos
  in foldr (\pos' -> M.insertWith (++) pos' [pos]) (M.delete pos dependencies) deps

-- | Collapse the wave function of a single position. Selects a random tile from the environment and
--  adds it to the tileMap. If the environment has no possible tiles, the function will return Nothing.
--  After the tile has been added to the tileMap, the position will be removed from the environment,
--  and the environment will be updated with the new possible tiles.
waveFuncCollapseStep :: Pos -> TileMap -> Env -> Dependencies -> History 
  -> IO (Maybe (TileMap, Env, Dependencies), History)
waveFuncCollapseStep pos (TileMap tileMap) env dependencies history  = do
  let (newTiles, weight, _) = env M.! pos
  rTile <- randomTile newTiles weight
  case rTile of
    Nothing -> return (Nothing, history)
    Just tile -> do 
      let newTileMap = M.insert pos tile tileMap
      let newHistory = HistoryUnit (TileMap tileMap) env dependencies pos tile : history
      let newEnv = M.delete pos env
      let newDependencies = updateDependencies pos tile (TileMap newTileMap) dependencies
      return (updateEnv (TileMap newTileMap) newEnv newDependencies, newHistory)

-- | Select a random tile from a list of tiles based on their weights.
--   If the total weight is 0, the function will return Nothing.
randomTile :: [Tile] -> Weights -> IO (Maybe Tile)
randomTile tiles (totalWeight, tilesWeight) = 
  if totalWeight == 0.0 then return Nothing else do
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
