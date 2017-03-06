-- |
-- Module: Graphics.OBJ.Parser
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.OBJ.Parser where

import           Control.Applicative             (empty)
import           Control.Monad                   (void)
import           Data.Scientific                 (toRealFloat)
import           Graphics.GL                     (GLfloat)
import           Linear                          (V2 (..), V3 (..))
import           Text.Megaparsec
import           Text.Megaparsec.ByteString.Lazy
import qualified Text.Megaparsec.Lexer           as Lexer

-- | Parse one vertex.
vertex :: Parser (V3 GLfloat)
vertex = v *> (V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat)

-- | Parse one normal.
normal :: Parser (V3 GLfloat)
normal = vn *> (V3 <$> lexeme glFloat <*> lexeme glFloat <*> lexeme glFloat)

-- Parse one texture coordinate.
texCoord :: Parser (V2 GLfloat)
texCoord = vt *> (V2 <$> lexeme glFloat <*> lexeme glFloat)

-- | Parse one GLfloat.
glFloat :: Parser GLfloat
glFloat = toRealFloat <$> Lexer.signed sc Lexer.scientific

-- | Parse the keyword "v" which initiates a vertex specification.
v :: Parser ()
v = void $ Lexer.symbol sc "v"

-- | Parse the keyword "vn" which initiates a vertex normal specification.
vn :: Parser ()
vn = void $ Lexer.symbol sc "vn"

-- | Parse the keyword "vt" which initiates a texture coordinate specification.
vt :: Parser ()
vt = void $ Lexer.symbol sc "vt"

-- | Space consumer; either whitespaces or line comments starting with '#'.
sc :: Parser ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "#") empty

-- | Lexeme; parser prepended with the space consumer.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
