-- |
-- Module: Graphics.OBJ.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
--
-- OBJ parser, where parts of the file just are parsed and converted to
-- Haskell types.
module Graphics.OBJ.Parser
    ( Part (..)
    , Face (..)
    , Elem (..)
    , fromFile
    ) where

import           Control.Applicative             (empty)
import           Control.Exception               (IOException, catch)
import           Control.Monad                   (void)
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Scientific                 (toRealFloat)
import           Graphics.GL                     (GLfloat)
import           Linear                          (V2 (..), V3 (..))
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy
import qualified Text.Megaparsec.Lexer           as Lexer

data Part
    = Vertex !(V3 GLfloat)
    | Normal !(V3 GLfloat)
    | TexCoord !(V2 GLfloat)
    | Face !Face
    deriving Show

data Face
    = Triangle !Elem !Elem !Elem
    deriving Show

data Elem
    = V !Int
    | VN !Int !Int
    | VTN !Int !Int !Int
    deriving (Eq, Ord, Show)

-- | Parse the file to parts.
fromFile :: FilePath -> IO (Either String [Part])
fromFile file = parseIt `catch` handler
    where
        parseIt :: IO (Either String [Part])
        parseIt = do
            content <- LBS.readFile file
            case runParser parts file content of
                Right ps -> return $ Right ps
                Left err -> return $ Left (show err)

        handler :: IOException -> IO (Either String [Part])
        handler = return . Left . show

parts :: Parser [Part]
parts = manyTill (sc *> part) eof

-- | Parse one part.
part :: Parser Part
part =  try vertex' <|> try normal' <|> try texCoord' <|> try face'
    where
        vertex' = Vertex <$> lexeme vertex
        normal' = Normal <$> lexeme normal
        texCoord' = TexCoord <$> lexeme texCoord
        face' = Face <$> lexeme triangle

-- | Parse one vertex.
vertex :: Parser (V3 GLfloat)
vertex = v *> (V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat)

-- | Parse one normal.
normal :: Parser (V3 GLfloat)
normal = vn *> (V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat)

-- Parse one texture coordinate.
texCoord :: Parser (V2 GLfloat)
texCoord = vt *> (V2 <$> lexeme glFloat <*> lexeme glFloat)

-- | Parse one triangle face.
triangle :: Parser Face
triangle =
    f *> (try vTriangle <|> try vnTriangle <|> try vtnTriangle)
    where
        vTriangle   = Triangle <$> vElem <*> vElem <*> vElem
        vnTriangle  = Triangle <$> vnElem <*> vnElem <*> vnElem
        vtnTriangle = Triangle <$> vtnElem <*> vtnElem <*> vtnElem

-- | Parse a vertex only element.
vElem :: Parser Elem
vElem = V <$> lexeme int

-- | Parse a vertex//normal element.
vnElem :: Parser Elem
vnElem = VN <$> lexeme int
            <*> ((lexeme (char '/')) >> lexeme (char '/') *> lexeme int)

-- | Parse a vertex/texture/normal element.
vtnElem :: Parser Elem
vtnElem = VTN <$> lexeme int
              <*> (lexeme (char '/') *> lexeme int)
              <*> (lexeme (char '/') *> lexeme int)

-- | Parse one GLfloat.
glFloat :: Parser GLfloat
glFloat = toRealFloat <$> Lexer.signed sc Lexer.scientific

-- | Parse one Int
int :: Parser Int
int = fromIntegral <$> Lexer.signed sc Lexer.integer

-- | Parse the keyword "v" which initiates a vertex specification.
v :: Parser ()
v = void $ Lexer.symbol sc "v"

-- | Parse the keyword "vn" which initiates a vertex normal specification.
vn :: Parser ()
vn = void $ Lexer.symbol sc "vn"

-- | Parse the keyword "vt" which initiates a texture coordinate specification.
vt :: Parser ()
vt = void $ Lexer.symbol sc "vt"

-- | Parse the keyword "f" which initiates a face element specification.
f :: Parser ()
f = void $ Lexer.symbol sc "f"

-- | Space consumer; either whitespaces or line comments starting with '#'.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; parser prepended with the space consumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
