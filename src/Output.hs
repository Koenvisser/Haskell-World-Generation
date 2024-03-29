module Output where

import Def

import qualified Data.Map as M
import Data.Default (def)
import Data.Maybe (maybeToList)
import System.Directory (createDirectoryIfMissing, copyFile)
import System.FilePath.Posix (takeFileName)

exampleWorld :: World
exampleWorld = World (((0,0,0), (1,1,1)), TileMap $ M.fromList [
  ((0,0,0), Tile (M.fromList [(PosX, def), (NegX, def {diffuseColor = (1,0,0)}), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def {texture = Just "C:/Users/KoenV/Downloads/texture.jpg"})]) (Rule (\_ _ -> CanPlace True)) 'a'),
  ((1,0,0), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'b'),
  ((0,1,0), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'c'),
  ((1,1,0), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'd'),
  ((0,0,1), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'e'),
  ((1,0,1), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'f'),
  ((0,1,1), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'g'),
  ((1,1,1), Tile (M.fromList [(PosX, def), (NegX, def), (PosY, def), (NegY, def), (PosZ, def), (NegZ, def)]) (Rule (\_ _ -> CanPlace True)) 'h')])

-- | Converts a world to an obj file, which can be used to render the world in a 3D renderer.
saveWorldToObj :: World -> FilePath -> IO ()
saveWorldToObj world path = let (objString, _, _) = undefined in writeFile path $ objString

saveWorldToObjAndMtl :: World -> FilePath -> IO ()
saveWorldToObjAndMtl world path = do
  let (objString, mtlString, files) = worldToObjAndMtl world
  createDirectoryIfMissing True path
  writeFile (path ++ "/obj.obj") $ "mtllib mat.mtl\n\n" ++ objString
  writeFile (path ++ "/mat.mtl") mtlString
  createDirectoryIfMissing True $ path ++ "/textures"
  mapM_ (\file -> copyFile file $ path ++ "/textures/" ++ (takeFileName file)) files
  

-- | Converts a world to a tuple of strings, where the first string is the obj file, the second string is the mtl file and 
--   the third is a list of file paths to the textures used in the mtl file
worldToObjAndMtl :: World -> (String, String, [FilePath])
worldToObjAndMtl (World (_, TileMap tileMap)) = 
  let (objString, _, mtlString, filePaths) = foldl (\(accObjString, fCount, accMtlString, accFiles) (pos, tile) -> 
                                let (newObjString, newfCount, newMtlString, files) = tileToObjAndMtl pos tile fCount 
                                in (accObjString ++ "\n" ++ newObjString, newfCount, accMtlString ++ "\n" ++ newMtlString, accFiles ++ files)
                             ) ("", 1, "", []) $ M.toList tileMap
  in (objString, mtlString, filePaths)

-- | Converts a tile to an obj file as a string, with a given face count and returns the new face count
tileToObjAndMtl :: Pos -> Tile -> Int -> (String, Int, String, [FilePath])
tileToObjAndMtl pos tile fCount | M.null $ materials tile = ("", fCount, "", []) 
                                | otherwise = let 
  verticeLines = map (\(x', y', z') -> "v " ++ show x' ++ " " ++ show y' ++ " " ++ show z') (vertices pos)
  facesWithMaterial = filter (\(_, _, _, _, side) -> M.member side $ materials tile) $ faces fCount
  name side = show pos ++ show side
  faceLines = map (\(v1, v2, v3, v4, side) -> "usemtl " ++ (name side) ++ "\nf " ++ show v1 ++ " " ++ show v2 ++ " " ++ show v3 ++ " " ++ show v4) facesWithMaterial
  (mtlString, files) = foldl (\(accMtlString, accFiles) (_, _, _, _, side) -> 
    let (newMtlString, newFiles) = materialToMtl (materials tile M.! side) (name side)
    in (accMtlString ++ "\n" ++ newMtlString, accFiles ++ newFiles)
    ) ("", []) facesWithMaterial
  in (unlines $ verticeLines ++ faceLines, fCount + 8, mtlString, files) 

materialToMtl :: Material -> String -> (String, [FilePath])
materialToMtl (Material (amR, amG, amB) (difR, difG, difB) (specR, specG, specB) transp specExp illum tex extraFields extraFiles) name = (unlines $ ["newmtl " ++ name,
  "Ka " ++ show amR ++ " " ++ show amG ++ " " ++ show amB,
  "Kd " ++ show difR ++ " " ++ show difG ++ " " ++ show difB,
  "Ks " ++ show specR ++ " " ++ show specG ++ " " ++ show specB,
  "d " ++ show transp,
  "Ns " ++ show specExp,
  "illum " ++ show illum] ++ maybe [] (\texPath -> ["map_Kd textures/" ++ (takeFileName texPath)]) tex ++ extraFields,
  extraFiles ++ maybeToList tex)

-- | Returns the 8 vertices of a tile, given the position of the tile
vertices :: (Int, Int, Int) -> [(Int, Int, Int)]
vertices (x, y, z) = [(x', y', z') | x' <- [x, x + 1], y' <- [y, y + 1], z' <- [z, z + 1]]

-- | Returns the 6 faces of a tile, given the face count, which is the index of the first vertex of the tile
--   The faces are represented as a list of 4 vertices and the side of the tile the face represents, 
--   where each vertex is the index of the vertex in the vertices list
faces :: Int -> [(Int, Int, Int, Int, Side)]
faces fCount = [
  (fCount,     fCount + 1, fCount + 3, fCount + 2, NegX), 
  (fCount + 4, fCount + 5, fCount + 7, fCount + 6, PosX), 
  (fCount,     fCount + 1, fCount + 5, fCount + 4, NegY), 
  (fCount + 2, fCount + 3, fCount + 7, fCount + 6, PosY), 
  (fCount,     fCount + 2, fCount + 6, fCount + 4, NegZ), 
  (fCount + 1, fCount + 3, fCount + 7, fCount + 5, PosZ)]
