{-# LANGUAGE MultiWayIf #-}
module Graphics.OBJ.Assembly
    ( basicAssembly
    ) where

import           Control.Monad.State             (State, evalState, get, put)
import           Data.List                       (foldl')
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Vector                     (Vector)
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

populateVTNMap :: Vector (V3 GLfloat)
               -> Vector (V3 GLfloat)
               -> Vector (V2 GLfloat)
               -> Vector Face
               -> State GLuint (Map Face (GLuint, VTN.Vertex))
populateVTNMap vertices normals texCoords faces = undefined

nextIndex :: State GLuint GLuint
nextIndex = do
    val <- get
    put (val + 1)
    return (val + 1)
