module Parser where

import Types

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

intExp = 
   do i <- integer
      return $ IntExp i

arOperators :: [[Operator Parser Exp]]
arOperators =
  [ [ InfixL (IntOpExp "*" <$ symbol "*")
    , InfixL (IntOpExp "/" <$ symbol "/") ]
  , [ InfixL (IntOpExp "+" <$ symbol "+")
    , InfixL (IntOpExp "-" <$ symbol "-") ]
  ]

arTerm :: Parser Exp
arTerm = intExp <|> arExp

arExp :: Parser Exp
arExp = makeExprParser arTerm arOperators


anExp :: Parser Exp
anExp = arExp

mainParser = anExp
