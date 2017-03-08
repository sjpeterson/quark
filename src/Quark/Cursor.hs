{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Quark.Cursor where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U

import Quark.Types ( Cursor
                   , Index
                   , Direction ( Backward
                               , Forward
                               , Up
                               , Down ) )
import Quark.Helpers ( nlTail
                     , strHeight
                     , (~~) )

-- Convert a linear index of a string to a cursor
ixToCursor :: Index -> ByteString -> Cursor
ixToCursor ix s = (row, col)
  where
    row = (length $ U.lines $ s0 ~~ " ") - 1
    col = (U.length $ last $ U.lines $ s0 ~~ " ") - 1
    (s0, _) = U.splitAt ix s

-- Convert a cursor on a string to a linear index
cursorToIx :: Cursor -> ByteString -> Index
cursorToIx _ "" = 0
cursorToIx (0, col) (U.uncons -> Just (x, xs))
    | col <= 0 || x == '\n' = 0
    | otherwise             = 1 + cursorToIx (0, col - 1) xs
cursorToIx (row, col) (U.uncons -> Just (x, xs))
    | row < 0   = 0
    | x == '\n' = 1 + cursorToIx (row - 1, col) xs
    | otherwise = 1 + cursorToIx (row, col) xs

-- Compute distance between two cursors on a string (may be negative)
distance :: Cursor -> Cursor -> ByteString -> Int
distance crs0 crs1 s = (cursorToIx crs1 s) - (cursorToIx crs0 s)

-- Order two cursors
minCursor :: Cursor -> Cursor -> Cursor
minCursor x y = case compare x y of
    GT -> y
    _  -> x

orderTwo :: Cursor -> Cursor -> (Cursor, Cursor)
orderTwo x y = case compare x y of
    GT -> (y, x)
    _  -> (x, y)

-- Move a cursor on a string
move :: Direction -> ByteString -> Cursor -> Cursor
move Backward s crs = ixToCursor (max ((cursorToIx crs s) - 1) 0) s
move Forward s crs = ixToCursor (min ((cursorToIx crs s) + 1) (U.length      s)) s
move Up _ (row, col) = (max (row - 1) 0, col)
move Down s (row, col) = (min (row + 1) $ strHeight s - 1, col)