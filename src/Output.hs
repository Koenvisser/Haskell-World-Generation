module Output where

import Def

import qualified Data.Map as M

-- | Converts a world to an obj file, which can be used to render the world in a 3D renderer.
saveWorldToObj :: World -> FilePath -> IO ()
saveWorldToObj world path = do
  writeFile path $ worldToObj world

-- | Converts a world to an obj file as a string
worldToObj :: World -> String
worldToObj (World (_, TileMap tileMap)) = 
  let (objString, _) = foldl (\(accObjString, fCount) (pos, tile) -> 
                                let (newObjString, newfCount) = tileToObj pos tile fCount 
                                in (accObjString ++ "\n" ++ newObjString, newfCount)
                             ) ("", 1) $ M.toList tileMap
  in objString

-- | Converts a tile to an obj file as a string, with a given face count and returns the new face count
tileToObj :: Pos -> Tile -> Int -> (String, Int)
tileToObj (x, y, z) _ fCount = (unlines $ 
  map (\(x', y', z') -> "v " ++ show x' ++ " " ++ show y' ++ " " ++ show z') vertices
  ++ map (\(v1, v2, v3, v4) -> "f " ++ show v1 ++ " " ++ show v2 ++ " " ++ show v3 ++ " " ++ show v4) faces 
  , fCount + 8) 
    where 
        vertices :: [(Int, Int, Int)]
        vertices = [(x', y', z') | x' <- [x, x + 1], y' <- [y, y + 1], z' <- [z, z + 1]]
        faces :: [(Int, Int, Int, Int)]
        faces = [
          (fCount,     fCount + 1, fCount + 3, fCount + 2), 
          (fCount + 4, fCount + 5, fCount + 7, fCount + 6), 
          (fCount,     fCount + 2, fCount + 6, fCount + 4), 
          (fCount + 1, fCount + 3, fCount + 7, fCount + 5), 
          (fCount,     fCount + 1, fCount + 5, fCount + 4), 
          (fCount + 2, fCount + 3, fCount + 7, fCount + 6)
          ]
