{-# LANGUAGE MultiWayIf #-}
module Graphics.OBJ.Assembly
    ( loadVTNFromFile
    ) where

import           Control.Monad                   (when)
import           Control.Monad.State             (State, evalState, get, put)
import           Data.List                       (foldl', sortBy)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Vector                     (Vector, (!))
import qualified Data.Vector                     as Vector
import qualified Data.Vector.Storable            as S
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

-- TODO: Look through the usage of vectors. At least the index vector could be
-- storable from the beginning.
loadVTNFromFile :: FilePath -> IO (Either String (S.Vector VTN.Vertex, S.Vector GLuint))
loadVTNFromFile file = do
    parseResult <- fromFile file
    case parseResult of
        Right parts ->
            case basicAssembly parts of
                VTNAssembly verts normals texCoords faces -> do
                    let (vs, is) = evalState (assembleVTN verts normals texCoords faces) emptyState
                    return $ Right (Vector.convert vs, Vector.convert is)

                BrokenAssembly -> return $ Left "Internal model inconsistency"

                _              -> return $ Left "Not a VTN model"

        Left err    -> return $ Left err

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
    Vector.fromList . reverse . foldl' (\acc x -> maybe acc (:acc) (f x)) []

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
        Triangle V {} V {} V {} -> True
        _                       -> False

vn :: Face -> Bool
vn face =
    case face of
        Triangle VN {} VN {} VN {} -> True
        _                          -> False

vtn :: Face -> Bool
vtn face =
    case face of
        Triangle VTN {} VTN {} VTN {} -> True
        _                             -> False

-- | Map from Elem to an entry of an assigned index and a vertex.
type AssemblyMap a = Map Elem (GLuint, a)

-- | State used during assembly of OBJ data structures.
data AssemblyState a = AssemblyState !GLuint !(AssemblyMap a)

-- | Make a fresh state.
emptyState :: AssemblyState a
emptyState = AssemblyState 0 Map.empty

assembleVTN :: Vector (V3 GLfloat)
            -> Vector (V3 GLfloat)
            -> Vector (V2 GLfloat)
            -> Vector Face
            -> State (AssemblyState VTN.Vertex) (Vector VTN.Vertex, Vector GLuint)
assembleVTN verts normals texCoords faces = do
    populateVTNMap verts normals texCoords faces
    vertices <- makeVertexVector
    indices <- makeIndexVector faces
    return (vertices, indices)

-- | Populate a Map with VTN vertices. Each entry maps to a unique key made up
-- from the face specification (with v/t/n). Each entry is assigned an index
-- value that will be used as index into a VBO.
populateVTNMap :: Vector (V3 GLfloat)
               -> Vector (V3 GLfloat)
               -> Vector (V2 GLfloat)
               -> Vector Face
               -> State (AssemblyState VTN.Vertex) ()
populateVTNMap vertices normals texCoords faces =
    Vector.forM_ faces $
        \(Triangle e1@(VTN v1 t1 n1) e2@(VTN v2 t2 n2) e3@(VTN v3 t3 n3)) -> do
            let vert1 = VTN.Vertex
                            { VTN.position = vertices ! (v1 - 1)
                            , VTN.normal = normals ! (n1 - 1)
                            , VTN.texCoord = texCoords ! (t1 - 1)
                            }
                vert2 = VTN.Vertex
                            { VTN.position = vertices ! (v2 - 1)
                            , VTN.normal = normals ! (n2 - 1)
                            , VTN.texCoord = texCoords ! (t2 - 1)
                            }
                vert3 = VTN.Vertex
                            { VTN.position = vertices ! (v3 - 1)
                            , VTN.normal = normals ! (n3 - 1)
                            , VTN.texCoord = texCoords ! (t3 - 1)
                            }
            insertIfNeeded e1 vert1
            insertIfNeeded e2 vert2
            insertIfNeeded e3 vert3

-- | Insert a vertex into the state if it's not already available.
insertIfNeeded :: Elem -> a -> State (AssemblyState a) ()
insertIfNeeded key vert = do
    AssemblyState cnt verts <- get
    when (Map.notMember key verts) $
        put $ AssemblyState (cnt + 1) (Map.insert key (cnt, vert) verts)

-- | Produce a vector out from the state. Each entry will fit into the index
-- it was assigned in the map.
makeVertexVector :: State (AssemblyState a) (Vector a)
makeVertexVector = do
    AssemblyState _ verts <- get
    return $ (Vector.fromList . map snd . sortBy (\(i1, _) (i2, _) -> i1 `compare` i2) . Map.elems) verts

makeIndexVector :: Vector Face -> State (AssemblyState a) (Vector GLuint)
makeIndexVector faces = do
    AssemblyState _ verts <- get
    return $ Vector.concatMap (visitFace verts) faces
    where
        visitFace :: AssemblyMap a -> Face -> Vector GLuint
        visitFace verts (Triangle e1 e2 e3) =
            Vector.fromList
                [ maybe 0 fst $ Map.lookup e1 verts
                , maybe 0 fst $ Map.lookup e2 verts
                , maybe 0 fst $ Map.lookup e3 verts
                ]
