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
lineSplitIx r s = (length s) - (length $ unlines $ drop r $ lines s) + 1

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