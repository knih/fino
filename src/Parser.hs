{-# LANGUAGE OverloadedStrings #-}
module Parser (parseExpr, test) where

import Syntax
import Lexer

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

nat :: Parser Expr
nat = do
  n <- (Token.natural lexer)
  return (ELit (LInt n))

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
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return $ foldr ELam body args

let_ :: Parser Expr
let_ = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet name (foldr ELam e1 args) e2

letrec :: Parser Expr
letrec = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (ELet name (EFix name (foldr ELam e1 args)) e2)

fix :: Parser Expr
fix = do
  reserved "fix"
  f <- identifier
  e <- expr
  return (EFix f e)

if_ :: Parser Expr
if_ = do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return (EIf e1 e2 e3)

arg :: Parser Expr
arg = parens expr <|> nat <|> bool <|> var

app :: Parser Expr
app = chainl1 arg (pure EApp)

binop :: Parser Expr
binop = buildExpressionParser table app
    where
      table =
          [[Infix (reservedOp "*"  *> pure (EOp Mul)) AssocLeft],
           [Infix (reservedOp "+"  *> pure (EOp Add)) AssocLeft,
            Infix (reservedOp "-"  *> pure (EOp Sub)) AssocLeft],
           [Infix (reservedOp "==" *> pure (EOp Eq))  AssocLeft,
            Infix (reservedOp ">"  *> pure (EOp Gt))  AssocLeft,
            Infix (reservedOp "<"  *> pure (EOp Lt))  AssocLeft],
           [Infix (reservedOp "&&" *> pure (EOp And)) AssocRight,
            Infix (reservedOp "||" *> pure (EOp Or))  AssocRight]]

letfunfix :: Parser Expr
letfunfix  = (try letrec) <|> let_ <|> fix <|> fun

expr :: Parser Expr
expr = letfunfix <|> if_ <|> binop

-- | Parser
parseExpr :: String -> Expr
parseExpr input = case parse (whiteSpace *> expr <* eof) "<stdin>" input of
                    Left err -> error $ "parse error at " ++ show err
                    Right e  -> e

test :: Show a => Parser a -> String -> IO ()
test p input = parseTest (do { whiteSpace
                            ; x <- p
                            ; eof
                            ; return x
                            }) input
