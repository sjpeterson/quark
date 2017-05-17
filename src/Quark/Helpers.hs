{-# LANGUAGE OverloadedStrings #-}

module Quark.Helpers where

import qualified Data.Text as T

import System.FilePath ( splitPath
                       , joinPath )
import Data.List ( isPrefixOf
                 , isSuffixOf
                 , isInfixOf )

import Quark.Types ( Size
                   , Index
                   , Match ( Exact
                           , Prefix
                           , Suffix
                           , Infix
                           , Negative ) )

-- T.Text version of (++)
(~~) :: T.Text -> T.Text -> T.Text
(~~) = T.append

-- XNOR
xnor :: Bool -> Bool -> Bool
xnor a b = (a && b) || (not a && not b)

-- Order two things, smallest first
orderTwo :: Ord a => a -> a -> (a, a)
orderTwo x y = case compare x y of
    GT -> (y, x)
    _  -> (x, y)

-- Compute width of a string in number of columns
strWidth :: T.Text -> Int
strWidth s = maximum $ map T.length $ T.lines s

-- Compute height of a string in number of rows
strHeight :: T.Text -> Int
strHeight s = length $ T.lines $ s ~~ " "

-- Compute size of a string (height and width)
strSize :: T.Text -> Size
strSize s = (strHeight s, strWidth s)

-- Line number width
lnWidth :: T.Text -> Int
lnWidth s =
    (length $ show $ (length $ T.lines s) + if nlTail s then 1 else 0) + 1

lnIndent :: Char -> Int -> T.Text -> Int
lnIndent c r s = case drop r (T.lines s) of
    (x:_) -> T.length $ T.takeWhile (\c' -> c' == c) x
    _     -> 0

lnIndent' :: Int -> T.Text -> T.Text
lnIndent' r s = case drop r (T.lines s) of
    (x:_) -> T.takeWhile isWhite x
    _     -> ""

isWhite :: Char -> Bool
isWhite c = if elem c [' ', '\t'] then True else False

lineSplitIx :: Int -> T.Text -> Int
lineSplitIx r s = min (T.length s) $ T.length $ T.unlines $ take r $ T.lines s

-- Check is a string ends in newline or not
nlTail :: T.Text -> Bool
nlTail "" = False
nlTail s  = if T.last s == '\n' then True else False

-- Pad a string with spaces at the end
padToLen :: Int -> T.Text -> T.Text
padToLen k a
    | k <= T.length a = a
    | otherwise       = padToLen k $ a ~~ " "

padToLen' :: Int -> T.Text -> T.Text
padToLen' k a
    | k <= T.length a = a
    | otherwise       = padToLen k (T.cons ' ' a)

-- Concatenate two strings, padding with spaces in the middle
padMidToLen :: Int -> String -> String -> String
padMidToLen k a0 a1
    | k >= (length a0 + length a1) = a0 ++ a1
    | otherwise                    = padMidToLen k (a0 ++ " ") a1

fixToLenPadMid :: Int -> [a] -> [a] -> a -> [a]
fixToLenPadMid k xs ys z
    | k <= 0      = []
    | nx > k      = take k xs
    | nx + ny > k = xs ++ take (k - nx) ys
    | otherwise   = fixToLenPadMid k xs (z:ys) z
  where
    nx = length xs
    ny = length ys

trimPathHead :: Int -> FilePath -> FilePath
trimPathHead k p
    | k <= 0        = ""
    | k >= length p = p
    | otherwise     = ".../" ++ (joinPath $
          dropWhile' (\x -> (length $ concat x) > (k - 4)) $ splitPath p)

dropWhile' :: ([a] -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xs = if f xs then dropWhile' f (drop 1 xs) else xs

-- Selection helpers
splitAt2 :: (Int, Int) -> T.Text -> (T.Text, T.Text, T.Text)
splitAt2 (k0, k1) l = (x, y, z)
  where
    (x, x') = T.splitAt k0 l
    (y, z) = T.splitAt (k1 - k0) x'

selectionLines :: (T.Text, T.Text, T.Text) -> [[(T.Text, Bool)]]
selectionLines (x, y, z) = case (nlTail x, nlTail y) of
    (True, True)   -> concat [lx, ly, lz]
    (True, False)  -> concat [lx, fuseSL ly lz]
    (False, True)  -> concat [fuseSL lx ly, lz]
    (False, False) -> fuseSL (fuseSL lx ly) lz
  where
    lx = [[(s, False)] | s <- T.lines x]
    ly = [[(s, True)] | s <- T.lines y]
    lz = [[(s, False)] | s <- T.lines z]
    fuseSL ps [] = ps
    fuseSL [] qs = qs
    fuseSL ps (q:qs) = ps' ++ (concat [p, q]):qs
      where
        (ps', [p]) = splitAt (length ps - 1) ps

dropSL :: Int -> [(T.Text, Bool)] -> [(T.Text, Bool)]
dropSL _ [] = []
dropSL k x@((p, q):xs)
    | k <= 0    = x
    | otherwise = case k < n of True  -> (T.drop k p, q):xs
                                False -> dropSL (k - n) xs
  where
    n = T.length p

padToLenSL :: Int -> [(T.Text, Bool)] -> [(T.Text, Bool)]
padToLenSL k [] = [(T.replicate k (T.singleton ' '), False)]
padToLenSL k (x@(p0, _):xs) = x:(padToLenSL (k - T.length p0) xs)

tabbedLength :: Int -> T.Text -> Int
tabbedLength tabWidth b = loop (0, 0) b
  where
    loop (n, nLine) xs = case T.uncons xs of
        Just ('\n', xs') -> loop (n + 1, 0) xs'
        Just ('\t', xs') -> loop (n + nn, nLine + nn) xs'
          where
            nn = tabWidth * (div (nLine + tabWidth) tabWidth) - nLine
        Just (_, xs')    -> loop (n + 1, nLine + 1) xs'
        Nothing          -> n

fixTo :: Int -> String -> String
fixTo k s = padToLenWith k ' ' $ take k s

padToLenWith :: Int -> a -> [a] -> [a]
padToLenWith k p x
    | k <= length x = x
    | otherwise     = padToLenWith k p (x ++ [p])

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a:as') (b:bs) (c:cs) (d:ds) = (a, b, c, d):(zip4 as' bs cs ds)
zip4 _       _      _      _      = []

findIx :: Index -> Bool -> T.Text -> T.Text -> Index
findIx _ _ findString s
    | T.isInfixOf findString s == False = (-1)
findIx k True findString s
    | s0b == "" && s1b == "" = k - 1
    | s1b == "" = T.length s0a
    | otherwise = T.length s0 + T.length s1a
  where
    (s0a, s0b) = T.breakOn findString s0
    (s1a, s1b) = T.breakOn findString s1
    (s0, s1) = T.splitAt k s
findIx k False findString s = if s0b == ""
                                  then T.length s0 + lastIx s1
                                  else lastIx s0
  where
    (s0a, s0b) = T.breakOn findString s0
    (s0, s1) = T.splitAt k s
    lastIx s' = case T.breakOn findString s' of
        (s'', "")  -> (-1)
        (s0', s1') -> T.length s0' + 1 + (lastIx $ T.drop 1 s1')

match :: (Eq a) => [a] -> [a] -> Match
match x y
    | x == y                 = Exact
    | isPrefixOf x y == True = Prefix
    | isSuffixOf x y == True = Suffix
    | isInfixOf x y == True  = Infix
    | otherwise              = Negative