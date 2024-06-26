-- | This module contains functions to convert a world to an obj file, which can be used to render the world in a 3D renderer.
--   The obj file can be saved, without materials, to a file using the `saveWorldToObj` function, or with materials to a folder using the `saveWorldToObjAndMtl` function.
module WorldGen.Output (saveHeightMapToImage, heightMapToImage, saveWorldToObj, saveWorldToObjAndMtl, worldToObj, worldToObjAndMtl) where

import WorldGen.Internal.Def
import WorldGen.Def

import qualified Data.Map as M
import Data.Maybe (maybeToList)
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist)
import System.FilePath.Posix (takeFileName, takeDirectory)
import Codec.Picture

-- | Saves a `HeightMap` to an image file, which can be used to visualize the height map
saveHeightMapToImage :: HeightMap 
                     -> Int       -- ^ The width of the image in pixels
                     -> Int       -- ^ The height of the image in pixels
                     -> FilePath  -- ^ The 'FilePath' to save the png file to
                     -> IO ()
saveHeightMapToImage hm w h path = do
  let directory = takeDirectory path
  putStrLn $ "Creating directory " ++ directory ++ " if it does not exist"
  createDirectoryIfMissing True directory
  putStrLn $ "Writing height map image to " ++ path
  writePng path $ heightMapToImage hm w h

-- | Converts a `HeightMap` to an image
heightMapToImage :: HeightMap 
                 -> Int -- ^ The width of the image in pixels
                 -> Int -- ^ The height of the image in pixels
                 -> Image Pixel16
heightMapToImage hm = generateImage (\x y -> let 
  in round $ 65535 * hm (fromIntegral x, fromIntegral y))

-- | Converts a `TileMap` to an obj file, which can be used to render the world in a 3D renderer.
--   The obj file is saved to the given path
saveWorldToObj :: TileMap
               -> FilePath  -- ^ The 'FilePath' to save the obj file to
               -> Float     -- ^ The scale of the faces of the tiles
               -> IO ()
saveWorldToObj tileMap path scale = do 
  let directory = takeDirectory path
  putStrLn $ "Creating directory " ++ directory ++ " if it does not exist"
  createDirectoryIfMissing True directory
  putStrLn $ "Writing obj file to " ++ path
  writeFile path $ worldToObj tileMap scale

-- | Converts a `TileMap` to an obj file and a mtl file, which can be used to render the world in a 3D renderer.
--   The obj file and mtl file are saved to the given path to a folder, and the textures are copied to a textures folder in the given path
saveWorldToObjAndMtl :: TileMap 
                     -> FilePath  -- ^ The 'FilePath' that refers to the folder where the obj and mtl files are saved, as well as the textures folder
                     -> Float     -- ^ The scale of the faces of the tiles, which is important for the texture scaling
                     -> IO ()
saveWorldToObjAndMtl tileMap path scale = do
  let (objString, mtlString, files) = worldToObjAndMtl tileMap scale
  putStrLn "Checking if all files exist..."
  mapM_ (\file -> do
    exists <- doesFileExist file
    if exists then return () else error $ "File " ++ file ++ " does not exist") files
  putStrLn $ "Creating directory " ++ path ++ " if it does not exist"
  createDirectoryIfMissing True path
  putStrLn $ "Writing obj file to " ++ path ++ "/obj.obj"
  writeFile (path ++ "/obj.obj") $ "mtllib mat.mtl\n\nvt 0 0\nvt 1 0\nvt 1 1\nvt 0 1\n" ++ objString
  putStrLn $ "Writing mtl file to " ++ path ++ "/mat.mtl"
  writeFile (path ++ "/mat.mtl") mtlString
  putStrLn $ "Copying textures to " ++ path ++ "/textures"
  createDirectoryIfMissing True $ path ++ "/textures"
  mapM_ (\file -> copyFile file $ path ++ "/textures/" ++ takeFileName file) files
  
-- | Converts a world to an obj file as a string, without any materials
worldToObj :: TileMap -> Float -> String
worldToObj (TileMap (tileMap, _)) scale = fst $ foldl (\(accObjString, fCount) (pos, tile) -> 
  let (objString, newfCount) = tileToObj pos tile scale fCount
  in (accObjString ++ "\n" ++ objString, newfCount)) ("", 1) $ M.toList tileMap

-- | Converts a world to a tuple of strings, where the first string is the obj file, the second string is the mtl file and 
--   the third is a list of file paths to the textures used in the mtl file
worldToObjAndMtl :: TileMap -> Float -> (String, String, [FilePath])
worldToObjAndMtl (TileMap (tileMap, _)) scale = 
  let (objString, _, mtlString, filePaths, _ ) = foldl (\(accObjString, fCount, accMtlString, accFiles, cache) (pos, tile) -> 
                                let (newObjString, newfCount, newMtlString, files, newCache) = tileToObjAndMtl pos tile scale fCount cache
                                in (accObjString ++ "\n" ++ newObjString, newfCount, accMtlString ++ newMtlString, accFiles ++ files, newCache)
                             ) ("", 1, "", [], M.empty) $ M.toList tileMap
  in (objString, mtlString, filePaths)

-- | Converts a tile to an obj file as a string, with a given face count and returns the new face count
--   Does not include materials
tileToObj :: Pos -> Tile -> Float -> Int -> (String, Int)
tileToObj pos _ scale fCount = let 
  -- Get the vertices of the tile, and convert each of them to a string
  verticeLines = scaleAndShowVertices scale $ vertices pos
  -- Convert the faces to a string, where each face is a string of 4 vertices. Also add the material to the face
  faceLines = map (\(v1, v2, v3, v4, _) -> "f " ++ show v1 ++ " " ++ show v2 ++ " " ++ show v3 ++ " " ++ show v4) (faces fCount)
  in (unlines $ verticeLines ++ faceLines, fCount + 8)

-- | Converts a tile to a tuple, where the first element is the obj file as a string, the second element is the new face count,
--   the third element is the mtl file as a string and the fourth element is a list of file paths to the textures used in the mtl file
tileToObjAndMtl :: Pos -> Tile -> Float -> Int -> M.Map Material String -> (String, Int, String, [FilePath], M.Map Material String)
tileToObjAndMtl pos tile scale fCount cache | M.null $ materials tile = ("", fCount, "", [], cache) 
                                            | otherwise = let 
  -- Get the vertices of the tile, and convert each of them to a string
  verticeLines = scaleAndShowVertices scale $ vertices pos
  -- Filter out the faces that don't have a material
  facesWithMaterial = filter (\(_, _, _, _, side) -> M.member side $ materials tile) $ faces fCount
  -- Name converts a side to a string, which is used to name the material
  name :: Side -> String
  name side = show pos ++ show side
  -- Convert the faces to a string, where each face is a string of 4 vertices. Also add the material to the face
  faceLine :: Int -> Int -> Int -> Int -> Side -> String -> String
  faceLine v1 v2 v3 v4 side mtlName =
    let (vt1, vt2, vt3, vt4) = textureCoords side 
    in "usemtl " ++ mtlName ++ "\nf " ++ show v1 ++ "/" ++ show vt1 ++ " " ++ show v2 ++ "/" ++ 
        show vt2 ++ " " ++ show v3 ++ "/" ++ show vt3 ++ " " ++ show v4 ++ "/" ++ show vt4
  -- Generate the mtl string and the list of files used in the mtl string
  (faceLines, mtlString, files, newCache) = foldl (\(accFaceLines, accMtlString, accFiles, accCache) (v1, v2, v3, v4, side) ->
    let material = materials tile M.! side
    in case M.lookup material accCache of
      Just mtlName -> (faceLine v1 v2 v3 v4 side mtlName : accFaceLines, accMtlString, accFiles, accCache)
      Nothing -> let
          mtlName = name side 
          (newMtlString, newFiles) = materialToMtl material mtlName
        in (faceLine v1 v2 v3 v4 side mtlName : accFaceLines, accMtlString ++ "\n" ++ newMtlString, accFiles ++ newFiles, M.insert material mtlName accCache)
    ) ([], "", [], cache) facesWithMaterial
  in (unlines $ verticeLines ++ faceLines, fCount + 8, mtlString, files, newCache) 

-- | Converts a material to a mtl file as a string, with a given name for the material 
--   and returns the mtl string and a list of file paths to the textures used in the mtl file
materialToMtl :: Material -> String -> (String, [FilePath])
materialToMtl 
  (Material (amR, amG, amB) (difR, difG, difB) (specR, specG, specB) transp specExp illum tex extrFields extrFiles) 
  name = (unlines $ ["newmtl " ++ name,
  "Ka " ++ show amR ++ " " ++ show amG ++ " " ++ show amB,
  "Kd " ++ show difR ++ " " ++ show difG ++ " " ++ show difB,
  "Ks " ++ show specR ++ " " ++ show specG ++ " " ++ show specB,
  "d " ++ show transp,
  "Ns " ++ show specExp,
  "illum " ++ show illum] ++ maybe [] (\texPath -> ["map_Kd textures/" ++ takeFileName texPath]) tex ++ extrFields,
  extrFiles ++ maybeToList tex)


-- | Returns the 8 vertices of a tile, given the position of the tile
vertices :: (Int, Int, Int) -> [(Int, Int, Int)]
vertices (x, y, z) = [(x', y', z') | x' <- [x, x + 1], y' <- [y, y + 1], z' <- [z, z + 1]]

-- | Scales and shows the vertices of a tile as a list of strings, where each string is a vertex
scaleAndShowVertices :: Float -> [(Int, Int, Int)] -> [String]
scaleAndShowVertices scale = map (\(x, y, z) -> "v " ++ show (fromIntegral x * scale) ++ " " ++ show (fromIntegral y * scale) ++ " " ++ show (fromIntegral z * scale))

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

-- | Returns the texture vertices for a side of a tile
textureCoords :: Side -> (Int, Int, Int, Int)
textureCoords PosX = (2, 1, 4, 3)
textureCoords NegX = (1, 2, 3, 4)
textureCoords PosY = (4, 1, 2, 3)
textureCoords NegY = (1, 4, 3, 2)
textureCoords PosZ = (1, 4, 3, 2)
textureCoords NegZ = (2, 3, 4, 1)
