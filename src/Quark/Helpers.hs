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

translateKey :: String -> Curses.Key
translateKey s = case s of
    "Return"    -> Curses.KeyChar '\r'
    "Backspace" -> Curses.KeyChar '\DEL'
    "Delete"    -> Curses.KeyDC
    "^Delete"   -> Curses.KeySDC
    "Tab"       -> Curses.KeyChar '\t'
    "^Tab"      -> Curses.KeyBTab
    "Home"      -> Curses.KeyHome
    "^Home"     -> Curses.KeySHome
    "C-Home"    -> Curses.KeyUnknown 537
    "C-^Home"   -> Curses.KeyUnknown 538
    "End"       -> Curses.KeyEnd
    "^End"      -> Curses.KeySEnd
    "C-End"     -> Curses.KeyUnknown 532
    "C-^End"    -> Curses.KeyUnknown 533
    "PgUp"      -> Curses.KeyPPage
    "^PgUp"     -> Curses.KeySPrevious
    "PgDn"      -> Curses.KeyNPage
    "^PgDn"     -> Curses.KeySNext
    "Insert"    -> Curses.KeyIC
    "Up"        -> Curses.KeyUp
    "C-Up"      -> Curses.KeyUnknown 569
    "C-^Up"     -> Curses.KeyUnknown 570
    "Down"      -> Curses.KeyDown
    "C-Down"    -> Curses.KeyUnknown 528
    "C-^Down"   -> Curses.KeyUnknown 529
    "Left"      -> Curses.KeyLeft
    "^Left"     -> Curses.KeySLeft
    "C-Left"    -> Curses.KeyUnknown 548
    "C-^Left"   -> Curses.KeyUnknown 549
    "Right"     -> Curses.KeyRight
    "^Right"    -> Curses.KeySRight
    "C-Right"   -> Curses.KeyUnknown 563
    "C-^Right"  -> Curses.KeyUnknown 564
    "C-a"       -> Curses.KeyChar '\SOH'
    "C-b"       -> Curses.KeyChar '\STX'
    "C-c"       -> Curses.KeyChar '\ETX'
    "C-d"       -> Curses.KeyChar '\EOT'
    "C-e"       -> Curses.KeyChar '\ENQ'
    "C-f"       -> Curses.KeyChar '\ACK'
    "C-g"       -> Curses.KeyChar '\a'
    "C-h"       -> Curses.KeyBackspace
    "C-i"       -> Curses.KeyChar '\t'
    "C-j"       -> Curses.KeyChar '\n'
    "C-k"       -> Curses.KeyChar '\v'
    "C-l"       -> Curses.KeyChar '\f'
    "C-m"       -> Curses.KeyChar '\r'
    "C-n"       -> Curses.KeyChar '\SO'
    "C-o"       -> Curses.KeyChar '\SI'
    "C-p"       -> Curses.KeyChar '\DLE'
    "C-q"       -> Curses.KeyChar '\DC1'
    "C-r"       -> Curses.KeyChar '\DC2'
    "C-s"       -> Curses.KeyChar '\DC3'
    "C-t"       -> Curses.KeyChar '\DC4'
    "C-u"       -> Curses.KeyChar '\NAK'
    "C-v"       -> Curses.KeyChar '\SYN'
    "C-w"       -> Curses.KeyChar '\ETB'
    "C-x"       -> Curses.KeyChar '\CAN'
    "C-y"       -> Curses.KeyChar '\EM'
    "C-z"       -> Curses.KeyChar '\SUB'