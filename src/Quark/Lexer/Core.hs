---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Core
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Core lexer function
--
---------------------------------------------------------------

module Quark.Lexer.Core where

{- data Token = Keyword String
           | Literal String
           | Whitespace String
           | Newline String
           | Operator String
           | Comment String
           | StringLiteral String
           | IntegerLiteral String
           | CharacterLiteral String
           | NumberLiteral String
           | BooleanLiteral String
           | Unclassified String deriving (Show, Eq) -}

-- Consider lens or microlens to make this neater
tokenLength :: Token -> Int
tokenLength (Keyword s) = length s
tokenLength (Literal s) = length s
tokenLength (Whitespace s) = length s

lexer :: Grammar -> String -> [Token]
lexer _ [] = []
lexer g s = t:(lexer g s')
  where
    t = nextToken g s
    s' = drop (tokenLength t) s

nextToken :: Grammar -> String -> Token
nextToken _ []           = []
nextToken [] x:_         = Unclassified x
nextToken g@(t, re):gs s = case s =~ re :: String of
    "" -> nextToken gs s
    s' -> t s'