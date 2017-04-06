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
-- Core lexer functions
--
---------------------------------------------------------------

module Quark.Lexer.Core ( RegexString
                        , Grammar
                        , CompiledGrammar
                        , lexer
                        , tokenString
                        , tokenLines
                        , tokenLength
                        , hintTabs
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

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as B

import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString

import Quark.Types
import Quark.Helpers ( (~~)
                     , nlTail
                     , padToLen )
import Quark.Colors ( defaultColor )
import Quark.Settings ( tabWidth )

type RegexString = ByteString

{- A grammar is a list of (Token data constructor, regex) tuples in order of
   precedence, see for example Quark.Lexer.Haskell -}
type Grammar = [(ByteString -> Token, RegexString)]
type CompiledGrammar = [(ByteString -> Token, ByteString -> ByteString)]

mapT :: (ByteString -> a) -> Token -> a
mapT f (Comment s)       = f s
mapT f (DocComment s)    = f s
mapT f (Pragma s)        = f s
mapT f (TypeIdent s)     = f s
mapT f (VarIdent s)      = f s
mapT f (ReservedIdent s) = f s
mapT f (Operator s)      = f s
mapT f (Bracket s)       = f s
mapT f (Separator s)     = f s
mapT f (IntLiteral s)    = f s
mapT f (FloatLiteral s)  = f s
mapT f (NumLiteral s)    = f s
mapT f (StringLiteral s) = f s
mapT f (CharLiteral s)   = f s
mapT f (BoolLiteral s)   = f s
mapT f (Whitespace s)    = f s
mapT f (Tabs s)          = f s
mapT f (Newline s)       = f s
mapT f (Decorator s)     = f s
mapT f (Symbol s)        = f s
mapT f (Unclassified s)  = f s

liftT :: (ByteString -> ByteString) -> Token -> Token
liftT f (Comment s)       = Comment $ f s
liftT f (DocComment s)    = DocComment $ f s
liftT f (Pragma s)        = Pragma $ f s
liftT f (TypeIdent s)     = TypeIdent $ f s
liftT f (VarIdent s)      = VarIdent $ f s
liftT f (ReservedIdent s) = ReservedIdent $ f s
liftT f (Operator s)      = Operator $ f s
liftT f (Bracket s)       = Bracket $ f s
liftT f (Separator s)     = Separator $ f s
liftT f (IntLiteral s)    = IntLiteral $ f s
liftT f (FloatLiteral s)  = FloatLiteral $ f s
liftT f (NumLiteral s)    = NumLiteral $ f s
liftT f (StringLiteral s) = StringLiteral $ f s
liftT f (CharLiteral s)   = CharLiteral $ f s
liftT f (BoolLiteral s)   = BoolLiteral $ f s
liftT f (Whitespace s)    = Whitespace $ f s
liftT f (Tabs s)          = Tabs $ f s
liftT f (Newline s)       = Newline $ f s
liftT f (Decorator s)     = Decorator $ f s
liftT f (Symbol s)        = Symbol $ f s
liftT f (Unclassified s)  = Unclassified $ f s

tokenString :: Token -> ByteString
tokenString = mapT id

tokenLength :: Token -> Int
tokenLength = mapT U.length

tokenLines :: [Token] -> [[Token]]
tokenLines [] = []
tokenLines t = cons (case break (== (Newline "\n")) t of
                         (l, t') -> (l, case t' of
                                            [] -> []
                                            _:t'' -> tokenLines t''))
  where
    cons ~(p, q) = p:q

hintTabs :: [Token] -> [Token]
hintTabs xs = loop 0 xs
  where
    loop _ [] = []
    loop k (y:ys) =
        case y of (Tabs s)    -> (Tabs s'):(loop (k + (U.length s')) ys)
                                   where
                                     s' = tabHint k $ U.length s
                  (Newline s) -> y:(loop 0 ys)
                  _           -> y:(loop (k + (U.length $ tokenString y)) ys)

tabHint :: Int -> Int -> ByteString
tabHint _ 0 = ""
tabHint k n = (padToLen kk "\226\135\165") ~~ (tabHint 0 (n-1))
  where
    kk = tabWidth - (mod k tabWidth)

splitT :: Token -> [Int] -> [Token]
splitT t []     = [t]
splitT t (s:ss) = t0:(splitT t1 $ map (\x -> x - s) ss)
  where
    t0 = liftT (U.take s) t
    t1 = liftT (U.drop s) t

takeTL :: Int -> [Token] -> [Token]
takeTL _ []     = []
takeTL n (t:ts) = case n > k of
    True  -> t:(takeTL (n-k) ts)
    False -> [liftT (U.take n) t]
  where
    k = tokenLength t

dropTL :: Int -> [Token] -> [Token]
dropTL _ [] = []
dropTL n (t:ts) = case n >= k of
    True  -> dropTL (n-k) ts
    False -> (liftT (U.drop n) t):ts
  where
    k = tokenLength t

compileGrammar :: Grammar -> CompiledGrammar
compileGrammar = map $ \(t, re) -> (t, matchCompiledRe re)

matchCompiledRe :: ByteString -> ByteString -> ByteString
matchCompiledRe re = match (makeRegex re :: Regex)

-- append word boundary check, suitable for reserved keywords
listToRe :: [String] -> RegexString
listToRe l = U.fromString $ "\\A(" ++ intercalate "|" wbL ++ ")"
  where
    wbL = map (\s -> s ++ "\\b") sortedL
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

-- do not append word boundary check, suitable for operators
listToRe' :: [String] -> RegexString
listToRe' l = U.fromString $ "\\A(" ++ intercalate "|" sortedL ++ ")"
  where
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

lexer :: CompiledGrammar -> ByteString -> [Token]
lexer g s = (fuseUnclassified . concat) $ lexer' g s

lexer' :: CompiledGrammar -> ByteString -> [[Token]]
lexer' _ ""   = []
lexer' _ "\n" = [[Newline "\n", Unclassified ""]]
lexer' g s    = t:(lexer' g s')
  where
    t = nextTokens g s
    s' = U.drop (sum (map tokenLength t)) s

nextTokens :: CompiledGrammar -> ByteString -> [Token]
nextTokens _ "" = []
nextTokens [] (U.uncons -> Just (x, _)) = [Unclassified $ U.fromString [x]]
nextTokens (g@(t, matchRe):gs) s = case matchRe s of
    "" -> nextTokens gs s
    s' -> case s' of
              "\n" -> [t s']
              _    -> tabSplit t $ intersperse (Newline "\n") $ map'' t sLines
                        where
                          sLines = U.lines s'

tabSplit :: (ByteString -> Token) -> [Token] -> [Token]
tabSplit _ []     = []
tabSplit t (x:xs) = case B.split '\t' (tokenString x) of
    ss@(_:_:_)  -> (intersperse (Tabs "\t") $ map'' t ss) ++ (tabSplit t xs)
    _           -> x:(tabSplit t xs)

map'' :: (ByteString -> Token) -> [ByteString] -> [Token]
map'' t xs = case xs of
    []      -> []
    (y:ys)  -> (t y):(map'' t ys)

hatify :: RegexString -> RegexString
hatify "" = ""
hatify re = case re =~ (U.fromString "\\\\A") :: Bool of
    True  -> re
    False -> "\\A" ~~ re

-- Not sure if this part is really worth it
fuseUnclassified :: [Token] -> [Token]
fuseUnclassified = foldr fuseFold []

fuseFold :: Token -> [Token] -> [Token]
fuseFold t [] = [t]
fuseFold (Unclassified s0) ((Unclassified s1):ts) =
    (Unclassified $ s0 ~~ s1):ts
fuseFold t ts = t:ts

tokenizeNothing :: ByteString -> [Token]
tokenizeNothing s = tabSplit Unclassified $ intersperse (Newline "\n") $
    [Unclassified s' | s' <- U.lines s ++ if nlTail s then [""] else []]

nothingColors :: Token -> Int
nothingColors _ = defaultColor -- 0