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
                        , tokenString
                        , tokenLines
                        , takeTL
                        , dropTL
                        , listToRe ) where

import Data.List ( intersperse
                 , intercalate
                 , sortBy )

import Text.Regex.PCRE

import qualified Quark.Types as Q

-- Consider lens or microlens to make this neater
mapT :: (String -> a) -> Q.Token -> a
mapT f (Q.Comment s)       = f s
mapT f (Q.DocComment s)    = f s
mapT f (Q.Pragma s)        = f s
mapT f (Q.TypeIdent s)     = f s
mapT f (Q.VarIdent s)      = f s
mapT f (Q.ReservedIdent s) = f s
mapT f (Q.Operator s)      = f s
mapT f (Q.Bracket s)       = f s
mapT f (Q.Separator s)     = f s
mapT f (Q.IntLiteral s)    = f s
mapT f (Q.FloatLiteral s)  = f s
mapT f (Q.NumLiteral s)    = f s
mapT f (Q.StringLiteral s) = f s
mapT f (Q.CharLiteral s)   = f s
mapT f (Q.BoolLiteral s)   = f s
mapT f (Q.Whitespace s)    = f s
mapT f (Q.Newline s)       = f s
mapT f (Q.Unclassified s)  = f s

liftT :: (String -> String) -> Q.Token -> Q.Token
liftT f (Q.Comment s)       = Q.Comment $ f s
liftT f (Q.DocComment s)    = Q.DocComment $ f s
liftT f (Q.Pragma s)        = Q.Pragma $ f s
liftT f (Q.TypeIdent s)     = Q.TypeIdent $ f s
liftT f (Q.VarIdent s)      = Q.VarIdent $ f s
liftT f (Q.ReservedIdent s) = Q.ReservedIdent $ f s
liftT f (Q.Operator s)      = Q.Operator $ f s
liftT f (Q.Bracket s)       = Q.Bracket $ f s
liftT f (Q.Separator s)     = Q.Separator $ f s
liftT f (Q.IntLiteral s)    = Q.IntLiteral $ f s
liftT f (Q.FloatLiteral s)  = Q.FloatLiteral $ f s
liftT f (Q.NumLiteral s)    = Q.NumLiteral $ f s
liftT f (Q.StringLiteral s) = Q.StringLiteral $ f s
liftT f (Q.CharLiteral s)   = Q.CharLiteral $ f s
liftT f (Q.BoolLiteral s)   = Q.BoolLiteral $ f s
liftT f (Q.Whitespace s)    = Q.Whitespace $ f s
liftT f (Q.Newline s)       = Q.Newline $ f s
liftT f (Q.Unclassified s)  = Q.Unclassified $ f s

tokenString :: Q.Token -> String
tokenString = mapT id

tokenLength :: Q.Token -> Int
tokenLength = mapT length

tokenLines :: [Q.Token] -> [[Q.Token]]
tokenLines [] = []
tokenLines t = cons (case break (== (Q.Newline "\n")) t of
                        (l, t') -> (l, case t' of
                                        [] -> []
                                        _:t'' -> tokenLines t''))
  where
    cons ~(p, q) = p:q

takeTL :: Int -> [Q.Token] -> [Q.Token]
takeTL _ []     = []
takeTL n (t:ts) = case n > k of
    True  -> t:(takeTL (n-k) ts)
    False -> [liftT (take n) t]
  where
    k = tokenLength t

dropTL :: Int -> [Q.Token] -> [Q.Token]
dropTL _ [] = []
dropTL n (t:ts) = case n >= k of
    True  -> dropTL (n-k) ts
    False -> (liftT (drop n) t):ts
  where
    k = tokenLength t

listToRe :: [String] -> Q.Regex
listToRe l = "^(" ++ intercalate "|" sortedL ++ ")"
  where
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

lexer :: Q.Grammar -> String -> [Q.Token]
lexer g s = (fuseUnclassified . concat) $ lexer' g s

lexer' :: Q.Grammar -> String -> [[Q.Token]]
lexer' _ [] = []
lexer' g s = t:(lexer' g s')
  where
    t = nextTokens g s
    s' = drop (sum (map tokenLength t) + (length t) - 1) s

nextTokens :: Q.Grammar -> String -> [Q.Token]
nextTokens _ []             = [Q.Unclassified ""]
nextTokens [] (x:xs)         = [Q.Unclassified [x]]
nextTokens (g@(t, re):gs) s = case s =~ (hatify re) :: String of
    "" -> nextTokens gs s
    s' -> case s' of
              "\n" -> [t s']
              _ -> [t s']
              -- _    -> intersperse (Q.Newline "\n") [t s'' | s'' <- lines s']

hatify :: Q.Regex -> Q.Regex
hatify "" = ""
hatify re@(x:xs) = case re =~ "\\\\A" :: Bool of
    True  -> re
    False -> "\\A" ++ re

-- Not sure if this part is really worth it
fuseUnclassified :: [Q.Token] -> [Q.Token]
fuseUnclassified = foldr fuseFold []

fuseFold :: Q.Token -> [Q.Token] -> [Q.Token]
fuseFold t [] = [t]
fuseFold (Q.Unclassified s0) ((Q.Unclassified s1):ts) =
    (Q.Unclassified $ s0 ++ s1):ts
fuseFold t ts = t:ts