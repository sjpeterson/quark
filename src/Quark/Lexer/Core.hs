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

import Quark.Types
import Quark.Helpers ((~~))
import Quark.Colors (defaultColor)

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
mapT f (Newline s)       = f s
mapT f (Decorator s)     = f s
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
liftT f (Newline s)       = Newline $ f s
liftT f (Decorator s)     = Decorator $ f s
liftT f (Unclassified s)  = Unclassified $ f s

tokenString :: Token -> ByteString
tokenString = mapT id

tokenLength :: Token -> Int
tokenLength = mapT B.length

tokenLines :: [Token] -> [[Token]]
tokenLines [] = []
tokenLines t = cons (case break (== (Newline "\n")) t of
                        (l, t') -> (l, case t' of
                                        [] -> []
                                        _:t'' -> tokenLines t''))
  where
    cons ~(p, q) = p:q

splitT :: Token -> [Int] -> [Token]
splitT t []     = [t]
splitT t (s:ss) = t0:(splitT t1 $ map (\x -> x - s) ss)
  where
    t0 = liftT (B.take s) t
    t1 = liftT (B.drop s) t

takeTL :: Int -> [Token] -> [Token]
takeTL _ []     = []
takeTL n (t:ts) = case n > k of
    True  -> t:(takeTL (n-k) ts)
    False -> [liftT (B.take n) t]
  where
    k = tokenLength t

dropTL :: Int -> [Token] -> [Token]
dropTL _ [] = []
dropTL n (t:ts) = case n >= k of
    True  -> dropTL (n-k) ts
    False -> (liftT (B.drop n) t):ts
  where
    k = tokenLength t

compileGrammar :: Grammar -> CompiledGrammar
compileGrammar = map $ \(t, re) -> (t, matchCompiledRe re)

matchCompiledRe :: ByteString -> ByteString -> ByteString
matchCompiledRe re = match (makeRegex re :: Regex)

-- append word boundary check, suitable for reserved keywords
listToRe :: [String] -> RegexString
listToRe l = B.pack $ "\\A(" ++ intercalate "|" wbL ++ ")"
  where
    wbL = map (\s -> s ++ "\\b") sortedL
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

-- do not append word boundary check, suitable for operators
listToRe' :: [String] -> RegexString
listToRe' l = B.pack $ "\\A(" ++ intercalate "|" sortedL ++ ")"
  where
    sortedL = sortBy (\x y -> compare (length y) (length x)) l

lexer :: CompiledGrammar -> ByteString -> [Token]
lexer g s = (fuseUnclassified . concat) $ lexer' g s

lexer' :: CompiledGrammar -> ByteString -> [[Token]]
lexer' _ "" = []
lexer' g s = t:(lexer' g s')
  where
    t = nextTokens g s
    s' = B.drop (sum (map tokenLength t)) s

nextTokens :: CompiledGrammar -> ByteString -> [Token]
nextTokens _ "" = []
nextTokens [] (B.uncons -> Just (x, _)) = [Unclassified $ B.pack [x]]
nextTokens (g@(t, matchRe):gs) s = case matchRe s of
    "" -> nextTokens gs s
    s' -> case s' of
              "\n" -> [t s']
              _    -> intersperse (Newline "\n") [t s'' | s'' <- sLines]
                        where
                          sLines = B.lines s'


hatify :: RegexString -> RegexString
hatify "" = ""
hatify re = case re =~ (B.pack "\\\\A") :: Bool of
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
tokenizeNothing s = intersperse (Newline "\n") $
    [Unclassified s' | s' <- B.lines s]

nothingColors :: Token -> Int
nothingColors _ = defaultColor -- 0