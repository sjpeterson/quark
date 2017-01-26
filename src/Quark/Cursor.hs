module Quark.Cursor where

import Quark.Types ( Cursor
                   , Index
                   , Direction ( Backward
                               , Forward
                               , Up
                               , Down ) )
import Quark.Helpers ( nlTail )

-- Convert a linear index of a string to a cursor
ixToCursor :: Index -> String -> Cursor
ixToCursor ix s = (row, col)
  where
    row = (length $ lines $ s0 ++ " ") - 1
    col = (length $ last $ lines $ s0 ++ " ") - 1
    (s0, _) = splitAt ix s

-- Convert a cursor on a string to a linear index
cursorToIx :: Cursor -> String -> Index
cursorToIx _ [] = 0
cursorToIx (0, col) (x:xs)
    | col <= 0 || x == '\n' = 0
    | otherwise             = 1 + cursorToIx (0, col - 1) xs
cursorToIx (row, col) (x:xs)
    | row < 0   = 0
    | x == '\n' = 1 + cursorToIx (row - 1, col) xs
    | otherwise = 1 + cursorToIx (row, col) xs

-- Compute distance between two cursors on a string (may be negative)
distance :: Cursor -> Cursor -> String -> Int
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
move :: Direction -> String -> Cursor -> Cursor
move Backward s crs = ixToCursor (max ((cursorToIx crs s) - 1) 0) s
move Forward s crs = ixToCursor (min ((cursorToIx crs s) + 1) (length s)) s
move Up _ (row, col) = (max (row - 1) 0, col)
move Down s (row, col) = (min (row + 1) rMax, col)
  where rMax = length (lines s) - if nlTail s then 0 else 1