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
    "let", "rec", "in",
    "fix",
    "if", "then", "else"
  ]

reservedOps :: [String]
reservedOps = [
    "->", "=",
    "+", "-", "*",
    ">","<",
    "&", "|"
  ]

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser $ Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = lower <|> char '_' :: Parser Char
  , Token.identLetter     = alphaNum <|> oneOf "_'" :: Parser Char
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = reservedNames
  , Token.reservedOpNames = reservedOps
  , Token.caseSensitive   = False
  }

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
