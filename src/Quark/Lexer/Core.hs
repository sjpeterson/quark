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

module Quark.Lexer.Core ( lexer
                        , listToRe ) where

import Data.List ( intersperse
                 , intercalate
                 , sortBy )

import Text.Regex.PCRE

import qualified Quark.Types as Q

-- Consider lens or microlens to make this neater
tokenString :: Q.Token -> String
tokenString (Q.Keyword s)          = s
tokenString (Q.Literal s)          = s
tokenString (Q.Whitespace s)       = s
tokenString (Q.Newline s)          = s
tokenString (Q.Operator s)         = s
tokenString (Q.Comment s)          = s
tokenString (Q.StringLiteral s)    = s
tokenString (Q.IntegerLiteral s)   = s
tokenString (Q.CharacterLiteral s) = s
tokenString (Q.FloatLiteral s)     = s
tokenString (Q.NumberLiteral s)    = s
tokenString (Q.BooleanLiteral s)   = s
tokenString (Q.Unclassified s)     = s

tokenLength :: Q.Token -> Int
tokenLength = length . tokenString

listToRe :: [String] -> Q.Regex
listToRe l = "^(" ++ intercalate "|" sortedL ++ ")"
  where
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

lexer :: Q.Grammar -> String -> [Q.Token]
lexer g s = concat $ lexer' g s

lexer' :: Q.Grammar -> String -> [[Q.Token]]
lexer' _ [] = []
lexer' g s = t:(lexer' g s')
  where
    t = nextTokens g s
    s' = drop (sum (map tokenLength t) + (length t) - 1) s

nextTokens :: Q.Grammar -> String -> [Q.Token]
nextTokens _ []             = [Q.Unclassified ""]
nextTokens [] (x:_)         = [Q.Unclassified [x]]
nextTokens (g@(t, re):gs) s = case s =~ (hatify re) :: String of
    "" -> nextTokens gs s
    s' -> case s' of
              "\n" -> [t s']
              _    -> intersperse (Q.Newline "\n") [t s'' | s'' <- lines s']

hatify :: Q.Regex -> Q.Regex
hatify "" = ""
hatify re@(x:xs) = case x of
    '^' -> re
    _   -> '^':re