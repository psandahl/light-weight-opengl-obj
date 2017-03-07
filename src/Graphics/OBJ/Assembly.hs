{-# LANGUAGE MultiWayIf #-}
module Graphics.OBJ.Assembly
    ( basicAssembly
    ) where

import           Control.Monad                   (when)
import           Control.Monad.State             (State, evalState, get, put)
import           Data.List                       (foldl', sortBy)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Vector                     (Vector, (!))
import qualified Data.Vector                     as Vector
import           Graphics.GL                     (GLfloat, GLuint)
import qualified Graphics.LWGL.Vertex_P_Norm_Tex as VTN
import           Graphics.OBJ.Parser             (Elem (..), Face (..),
                                                  Part (..), fromFile)
import           Linear                          (V2, V3)

data Assembly
    = VAssembly !(Vector (V3 GLfloat)) !(Vector Face)
    | VNAssembly !(Vector (V3 GLfloat)) !(Vector (V3 GLfloat)) !(Vector Face)
    | VTNAssembly !(Vector (V3 GLfloat)) !(Vector (V3 GLfloat)) !(Vector (V2 GLfloat)) !(Vector Face)
    | BrokenAssembly
    deriving Show

-- | From the parsed file parts, make a basic assembly to data structures from
-- where real vertex and index data can be created. The basic assembly also
-- performs a few sanity checks.
basicAssembly :: [Part] -> Assembly
basicAssembly xs =
    let (vertices, normals, texCoords, faces) = partition xs
    in
        if  | faceType v faces &&
                Vector.null normals &&
                Vector.null texCoords ->
                    VAssembly vertices faces
            | faceType vn faces &&
                not (Vector.null normals) &&
                Vector.null texCoords  ->
                VNAssembly vertices normals faces
            | faceType vtn faces &&
                not (Vector.null normals) &&
                not (Vector.null texCoords) ->
                VTNAssembly vertices normals texCoords faces
            | otherwise -> BrokenAssembly

partition :: [Part] -> ( Vector (V3 GLfloat), Vector (V3 GLfloat)
                       , Vector (V2 GLfloat), Vector Face )
partition xs = ( filterPart filterVertex xs, filterPart filterNormal xs
               , filterPart filterTexCoord xs, filterPart filterFace xs )

filterPart :: (a -> Maybe b) -> [a] -> Vector b
filterPart f =
    Vector.fromList . reverse . foldl' (\acc x -> maybe acc (\x' -> x':acc) (f x)) []

filterVertex :: Part -> Maybe (V3 GLfloat)
filterVertex part =
    case part of
        Vertex ve -> Just ve
        _         -> Nothing

filterNormal :: Part -> Maybe (V3 GLfloat)
filterNormal part =
    case part of
        Normal n -> Just n
        _        -> Nothing

filterTexCoord :: Part -> Maybe (V2 GLfloat)
filterTexCoord part =
    case part of
        TexCoord t -> Just t
        _          -> Nothing

filterFace :: Part -> Maybe Face
filterFace part =
    case part of
        Face f -> Just f
        _      -> Nothing

faceType :: (Face -> Bool) -> Vector Face -> Bool
faceType g = Vector.foldl' (\acc face -> acc && g face) True

v :: Face -> Bool
v face =
    case face of
        Triangle (V _) (V _) (V _ ) -> True
        _                           -> False

vn :: Face -> Bool
vn face =
    case face of
        Triangle (VN _ _) (VN _ _) (VN _ _) -> True
        _                                   -> False

vtn :: Face -> Bool
vtn face =
    case face of
        Triangle (VTN _ _ _) (VTN _ _ _) (VTN _ _ _) -> True
        _                                            -> False

type AssemblyMap a = Map Elem (GLuint, a)

data AssemblyState a = AssemblyState !GLuint !(AssemblyMap a)

populateVTNMap :: Vector (V3 GLfloat)
               -> Vector (V3 GLfloat)
               -> Vector (V2 GLfloat)
               -> Vector Face
               -> State (AssemblyState VTN.Vertex) ()
populateVTNMap vertices normals texCoords faces =
    Vector.forM_ faces $
        \(Triangle e1@(VTN v1 t1 n1) e2@(VTN v2 t2 n2) e3@(VTN v3 t3 n3)) -> do
            let vert1 = VTN.Vertex
                            { VTN.position = vertices ! v1 + 1
                            , VTN.normal = normals ! n1 + 1
                            , VTN.texCoord = texCoords ! t1 + 1
                            }
                vert2 = VTN.Vertex
                            { VTN.position = vertices ! v2 + 1
                            , VTN.normal = normals ! n2 + 1
                            , VTN.texCoord = texCoords ! t2 + 1
                            }
                vert3 = VTN.Vertex
                            { VTN.position = vertices ! v3 + 1
                            , VTN.normal = normals ! n3 + 1
                            , VTN.texCoord = texCoords ! t3 + 1
                            }
            insertIfNeeded e1 vert1
            insertIfNeeded e2 vert2
            insertIfNeeded e3 vert3

insertIfNeeded :: Elem -> a -> State (AssemblyState a) ()
insertIfNeeded key vert = do
    AssemblyState cnt verts <- get
    when (Map.notMember key verts) $
        put $ AssemblyState (cnt + 1) (Map.insert key (cnt, vert) verts)

mapToVector :: State (AssemblyState a) (Vector a)
mapToVector = do
    AssemblyState _ verts <- get
    return $ (Vector.fromList . map snd . sortBy (\(i1, _) (i2, _) -> i1 `compare` i2) . Map.elems) verts
