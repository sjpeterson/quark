{-# LANGUAGE OverloadedStrings #-}

module Quark.Helpers where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import qualified UI.HSCurses.Curses as Curses

import Quark.Types (Size)

-- ByteString version of (++)
(~~) :: B.ByteString -> B.ByteString -> B.ByteString
(~~) = B.append

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
lnWidth :: ByteString -> Int
lnWidth s = (length $ show $ length $ B.lines s) + 1

lnIndent :: Int -> ByteString -> Int
lnIndent r s = case drop r (B.lines s) of
    (x:_) -> B.length $ B.takeWhile (\c -> c == ' ') x
    _     -> 0

lineSplitIx :: Int -> ByteString -> Int
lineSplitIx r s = min (B.length s) $ B.length $ B.unlines $ take r $ B.lines s

-- Check is a string ends in newline or not
nlTail :: ByteString -> Bool
nlTail "" = False
nlTail s  = if B.last s == '\n' then True else False

-- Pad a string with spaces at the end
padToLen :: Int -> ByteString -> ByteString
padToLen k a
    | k <= B.length a = a
    | otherwise     = padToLen k $ a ~~ " "

padToLen' :: Int -> ByteString -> ByteString
padToLen' k a
    | k <= B.length a = a
    | otherwise     = padToLen k (B.cons ' ' a)

-- Concatenate two strings, padding with spaces in the middle
padMidToLen :: Int -> String -> String -> String
padMidToLen k a0 a1
    | k == (length a0 + length a1) = a0 ++ a1
    | otherwise                    = padMidToLen k (a0 ++ " ") a1

-- Selection helpers
splitAt2 :: (Int, Int) -> ByteString -> (ByteString, ByteString, ByteString)
splitAt2 (k0, k1) l = (x, y, z)
  where
    (x, x') = B.splitAt k0 l
    (y, z) = B.splitAt (k1 - k0) x'

selectionLines :: (ByteString, ByteString, ByteString) -> [[(ByteString, Bool)]]
selectionLines (x, y, z) = case (nlTail x, nlTail y) of
    (True, True)   -> concat [lx, ly, lz]
    (True, False)  -> concat [lx, fuseSL ly lz]
    (False, True)  -> concat [fuseSL lx ly, lz]
    (False, False) -> fuseSL (fuseSL lx ly) lz
  where
    lx = [[(s, False)] | s <- B.lines x]
    ly = [[(s, True)] | s <- B.lines y]
    lz = [[(s, False)] | s <- B.lines z]
    fuseSL ps [] = ps
    fuseSL [] qs = qs
    fuseSL ps (q:qs) = ps' ++ (concat [p, q]):qs
      where
        (ps', [p]) = splitAt (length ps - 1) ps

dropSL :: Int -> [(ByteString, Bool)] -> [(ByteString, Bool)]
dropSL _ [] = []
dropSL k x@((p, q):xs)
    | k <= 0    = x
    | otherwise = case k < n of True  -> (B.drop k p, q):xs
                                False -> dropSL (k - n) xs
  where
    n = B.length p

padToLenSL :: Int -> [(ByteString, Bool)] -> [(ByteString, Bool)]
padToLenSL k [] = [(B.replicate k ' ', False)]
padToLenSL k (x@(p0, _):xs) = x:(padToLenSL (k - B.length p0) xs)

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a:as') (b:bs) (c:cs) (d:ds) = (a, b, c, d):(zip4 as' bs cs ds)
zip4 _       _      _      _      = []