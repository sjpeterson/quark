{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Window.Core
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Window.Core lexer function
--
---------------------------------------------------------------

module Quark.Lexer.Core ( lexer
                        , tokenString
                        , tokenLines
                        , tokenLength
                        , splitT
                        , takeTL
                        , dropTL
                        , compileGrammar
                        , listToRe
                        , listToRe'
                        , tokenizeNothing
                        , nothingColors ) where

import Data.List ( intersperse
                 , intercalate
                 , sortBy )

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString

import qualified Quark.Types as Q
import Quark.Helpers ((~~))
import Quark.Colors (defaultColor)

-- Consider lens or microlens to make this neater
mapT :: (ByteString -> a) -> Q.Token -> a
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

liftT :: (ByteString -> ByteString) -> Q.Token -> Q.Token
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

tokenString :: Q.Token -> ByteString
tokenString = mapT id

tokenLength :: Q.Token -> Int
tokenLength = mapT B.length

tokenLines :: [Q.Token] -> [[Q.Token]]
tokenLines [] = []
tokenLines t = cons (case break (== (Q.Newline "\n")) t of
                        (l, t') -> (l, case t' of
                                        [] -> []
                                        _:t'' -> tokenLines t''))
  where
    cons ~(p, q) = p:q

splitT :: Q.Token -> [Int] -> [Q.Token]
splitT t []     = [t]
splitT t (s:ss) = t0:(splitT t1 $ map (\x -> x - s) ss)
  where
    t0 = liftT (B.take s) t
    t1 = liftT (B.drop s) t

takeTL :: Int -> [Q.Token] -> [Q.Token]
takeTL _ []     = []
takeTL n (t:ts) = case n > k of
    True  -> t:(takeTL (n-k) ts)
    False -> [liftT (B.take n) t]
  where
    k = tokenLength t

dropTL :: Int -> [Q.Token] -> [Q.Token]
dropTL _ [] = []
dropTL n (t:ts) = case n >= k of
    True  -> dropTL (n-k) ts
    False -> (liftT (B.drop n) t):ts
  where
    k = tokenLength t

compileGrammar :: Q.Grammar -> Q.CompiledGrammar
compileGrammar = map $ \(t, re) -> (t, matchCompiledRe re)

matchCompiledRe :: ByteString -> ByteString -> ByteString
matchCompiledRe re = match (makeRegex re :: Regex)

-- append word boundary check, suitable for reserved keywords
listToRe :: [String] -> Q.Regex
listToRe l = B.pack $ "\\A(" ++ intercalate "|" wbL ++ ")"
  where
    wbL = map (\s -> s ++ "\\b") sortedL
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

-- do not append word boundary check, suitable for operators
listToRe' :: [String] -> Q.Regex
listToRe' l = B.pack $ "\\A(" ++ intercalate "|" sortedL ++ ")"
  where
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

lexer :: Q.CompiledGrammar -> ByteString -> [Q.Token]
lexer g s = (fuseUnclassified . concat) $ lexer' g s

lexer' :: Q.CompiledGrammar -> ByteString -> [[Q.Token]]
lexer' _ "" = []
lexer' g s = t:(lexer' g s')
  where
    t = nextTokens g s
    s' = B.drop (sum (map tokenLength t)) s

nextTokens :: Q.CompiledGrammar -> ByteString -> [Q.Token]
nextTokens _ "" = []
nextTokens [] (B.uncons -> Just (x, _)) = [Q.Unclassified $ B.pack [x]]
nextTokens (g@(t, matchRe):gs) s = case matchRe s of
    "" -> nextTokens gs s
    s' -> case s' of
              "\n" -> [t s']
              _    -> intersperse (Q.Newline "\n") [t s'' | s'' <- sLines]
                        where
                          sLines = B.lines s'


hatify :: Q.Regex -> Q.Regex
hatify "" = ""
hatify re = case re =~ (B.pack "\\\\A") :: Bool of
    True  -> re
    False -> "\\A" ~~ re

-- Not sure if this part is really worth it
fuseUnclassified :: [Q.Token] -> [Q.Token]
fuseUnclassified = foldr fuseFold []

fuseFold :: Q.Token -> [Q.Token] -> [Q.Token]
fuseFold t [] = [t]
fuseFold (Q.Unclassified s0) ((Q.Unclassified s1):ts) =
    (Q.Unclassified $ s0 ~~ s1):ts
fuseFold t ts = t:ts

tokenizeNothing :: ByteString -> [Q.Token]
tokenizeNothing s = intersperse (Q.Newline "\n") $
    [Q.Unclassified s' | s' <- B.lines s]

nothingColors :: Q.Token -> Int
nothingColors _ = defaultColor -- 0