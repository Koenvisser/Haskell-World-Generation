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

-- | Weights is a tuple of a float and a list of floats (Total weight, [Weights])
type Weights = (Float, [Float])
type ShannonEntropy = Float
type Env = M.Map Pos ([Tile], Weights, ShannonEntropy)


waveFuncCollapse :: [Tile] -> Size -> IO TileMap
waveFuncCollapse tiles size@((minX, minY, minZ), (maxX, maxY, maxZ)) = do
  let allPos = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
  let emptyTileMap = TileMap M.empty
  let env = createEnv tiles emptyTileMap allPos
  waveFuncCollapse' emptyTileMap env

createEnv :: [Tile] -> TileMap -> [Pos] -> Env
createEnv tiles tileMap = M.fromList . (map posToEnv) 
  where
    posToEnv :: Pos -> (Pos, ([Tile], Weights, ShannonEntropy))
    posToEnv pos = do
      let weights = map (resultToFloat . (\(Rule f) -> f tileMap pos) . rules) tiles
      let newTiles = map fst $ filter (\m -> snd m > 0) $ zip tiles weights      
      let totWeight = sum weights
      (pos, (newTiles, (totWeight, weights), shannonEntropy (totWeight, weights)))

shannonEntropy :: Weights -> ShannonEntropy
shannonEntropy (totWeight, weights) = log totWeight - (h / totWeight)
  where
    h = foldr (\weight prev -> prev + (weight *  log weight)) 0 weights


waveFuncCollapse' :: TileMap -> Env -> IO TileMap
waveFuncCollapse' tileMap env
  | M.null env = return tileMap
  | otherwise = do
    randomPos <- shannonPos env
    waveFuncCollapseStep randomPos tileMap env >>= \case
      Nothing -> resetWaveFuncCollapse
      Just (newTileMap, newEnv) -> waveFuncCollapse' newTileMap newEnv

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

waveFuncCollapseStep :: Pos -> TileMap -> Env -> IO (Maybe (TileMap, Env))
waveFuncCollapseStep pos (TileMap tileMap) env = do
  let (tiles, weight, _) = env M.! pos
  tile <- randomTile tiles weight
  case tile of
    Nothing -> return Nothing
    Just tile -> return $ do
      (newTilesMap, newTileMap) <- do
        let newTilesMap = 
      -- Apply tile to tilemap
      -- Remove position from env
      -- roep createEnv aan



        -- M.foldrWithKey (\p tiles maps -> case maps of
        -- Nothing -> Nothing
        -- Just (newTilesMap, newTileMap) -> 
        --   if p == pos then Just (newTilesMap, M.insert p tile newTileMap) else do
        --         let newTiles = filter ((\(Rule f) -> resultToBool (f (TileMap tileMap) pos)) . rules) tiles
        --         case newTiles of
        --           [] -> Nothing
        --           [tile] -> Just (newTilesMap, M.insert p tile newTileMap)
        --           _ -> Just (M.insert p newTiles newTilesMap, newTileMap)
        --           -- calc weights
        --           -- calc shannon entropy
        --     ) (Just (M.empty, tileMap)) tilesMap
      return (TileMap newTileMap, newTilesMap)

updateEnv :: [Tile] -> TileMap -> [Pos] -> Maybe (TileMap, Env)
updateEnv tiles tileMap poss = do 
  envList <- (map posToEnv)
  M.fromList envList
  where
    posToEnv :: Pos -> (Pos, ([Tile], Weights, ShannonEntropy))
    posToEnv pos = do
      let weights = map (resultToFloat . (\(Rule f) -> f tileMap pos) . rules) tiles
      let newTiles = map fst $ filter (\m -> snd m > 0) $ zip tiles weights
      case newTiles of
        [] -> Nothing
        [tile] -> Just 
        _ -> do
          totWeight = sum weights
          (pos, (newTiles, (totWeight, weights), shannonEntropy (totWeight, weights)))


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
