module Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Expr
import Data.Functor.Identity

type Op a = Expr.Operator String () Identity a

reservedNames :: [String]
reservedNames = [
    "fun",
    "let",
    "in",
    "fix",
    "if",
    "then",
    "else"
  ]

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "=",
    ">",
    "<",
    "&",
    "|",
    "="
  ]

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser $ Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = lower <|> char '_'
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = reservedNames
  , Token.reservedOpNames = reservedOps
  , Token.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

semi :: Parser String
semi = Token.semi lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
