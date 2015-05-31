{-# LANGUAGE OverloadedStrings #-}
module Parser (parseExpr) where

import Syntax
import Lexer

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import qualified Data.Text.Lazy as L
import Data.Functor.Identity

whiteSpace :: Parser () 
whiteSpace = Token.whiteSpace lexer

int :: Parser Expr
int = do
  n <- (Token.integer lexer)
  return (ELit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = (reserved "True" >> return (ELit (LBool True)))
       <|> (reserved "False" >> return (ELit (LBool False)))

var :: Parser Expr
var = do
  x <- identifier
  return (EVar x)

fun :: Parser Expr
fun = do
  reserved "fun"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr ELam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (ELet x e1 e2)

fix :: Parser Expr
fix = do
  reserved "fix"
  f <- identifier
  e <- term
  return (EFix f e)

if_ :: Parser Expr
if_ = do
  reserved "if"
  e <- expr
  reserved "then"
  e1 <- expr
  reserved "else"
  e2 <- expr
  return (EIf e e1 e2)

term :: Parser Expr
term = parens expr
       <|> int
       <|> bool
       <|> var
       <|> letin
       <|> fun
       <|> fix
       <|> if_

binop :: Parser Expr
binop = buildExpressionParser table term
    where
      table =
          [[Infix (reservedOp "*" *> pure (EOp Mul)) AssocLeft],
           [Infix (reservedOp "+" *> pure (EOp Add)) AssocLeft,
            Infix (reservedOp "-" *> pure (EOp Sub)) AssocLeft],
           [Infix (reservedOp "==" *> pure (EOp Eq)) AssocLeft,
            Infix (reservedOp ">"  *> pure (EOp Gt)) AssocLeft,
            Infix (reservedOp "<"  *> pure (EOp Lt)) AssocLeft],
           [Infix (reservedOp "&&" *> pure (EOp And)) AssocRight,
            Infix (reservedOp "||" *> pure (EOp Or)) AssocRight]]

expr :: Parser Expr
expr = chainl1 binop (return EApp)

parseExpr :: String -> IO Expr
parseExpr input = case parse (whiteSpace *> expr <* eof) "<stdin>" input of
                    Left err -> fail $ "parse error at " ++ show err
                    Right e  -> return e
