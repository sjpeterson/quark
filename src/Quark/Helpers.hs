module Quark.Helpers where

import Quark.Types (Size)

-- Compute width of a string in number of columns
strWidth :: String -> Int
strWidth s = maximum $ map length $ lines s

-- Compute height of a string in number of rows
strHeight :: String -> Int
strHeight s = length $ lines $ s ++ " "

-- Compute size of a string (height and width)
strSize :: String -> Size
strSize s = (strHeight s, strWidth s)

-- Line number width
lnWidth :: String -> Int
lnWidth s = (length $ show $ length $ lines s) + 1

lnIndent :: Int -> String -> Int
lnIndent r s = case drop r (lines s) of
    (x:_) -> length $ takeWhile (\c -> c == ' ') x
    _     -> 0

lineSplitIx :: Int -> String -> Int
lineSplitIx r s = min (length s) $ length $ unlines $ take r $ lines s

-- Check is a string ends in newline or not
nlTail :: String -> Bool
nlTail "" = False
nlTail s  = if last s == '\n' then True else False

-- Pad a string with spaces at the end
padToLen :: Int -> [Char] -> [Char]
padToLen k a
    | k <= length a = a
    | otherwise     = padToLen k $ a ++ " "

padToLen' :: Int -> [Char] -> [Char]
padToLen' k a
    | k <= length a = a
    | otherwise     = padToLen k (' ':a)

-- Concatenate two strings, padding with spaces in the middle
padMidToLen :: Int -> [Char] -> [Char] -> [Char]
padMidToLen k a0 a1
    | k == (length a0 + length a1) = a0 ++ a1
    | otherwise                    = padMidToLen k (a0 ++ " ") a1

-- Selection helpers
splitAt2 :: (Int, Int) -> [a] -> ([a], [a], [a])
splitAt2 (k0, k1) l = (x, y, z)
  where
    (x, x') = splitAt k0 l
    (y, z) = splitAt (k1 - k0) x'

selectionLines :: (String, String, String) -> [[(String, Bool)]]
selectionLines (x, y, z) = case (nlTail x, nlTail y) of
    (True, True)   -> concat [lx, ly, lz]
    (True, False)  -> concat [lx, fuseSL ly lz]
    (False, True)  -> concat [fuseSL lx ly, lz]
    (False, False) -> fuseSL (fuseSL lx ly) lz
  where
    lx = [[(s, False)] | s <- lines x]
    ly = [[(s, True)] | s <- lines y]
    lz = [[(s, False)] | s <- lines z]
    fuseSL ps [] = ps
    fuseSL [] qs = qs
    fuseSL ps (q:qs) = ps' ++ (concat [p, q]):qs
      where
        (ps', [p]) = splitAt (length ps - 1) ps

dropSL :: Int -> [(String, Bool)] -> [(String, Bool)]
dropSL _ [] = []
dropSL k x@((p, q):xs)
    | k <= 0    = x
    | otherwise = case k < n of True  -> (drop k p, q):xs
                                False -> dropSL (k - n) xs
  where
    n = length p

padToLenSL :: Int -> [(String, Bool)] -> [(String, Bool)]
padToLenSL k [] = [(replicate k ' ', False)]
padToLenSL k (x@(p0, _):xs) = x:(padToLenSL (k - length p0) xs)