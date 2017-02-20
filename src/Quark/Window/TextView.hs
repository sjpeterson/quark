{-# LANGUAGE OverloadedStrings #-}

--------
--
-- Module:      Quark.Window.TextView
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for functions related to text view windows
--
--------

module Quark.Window.TextView where

import qualified UI.HSCurses.Curses as Curses

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Quark.Lexer.Core
import Quark.Lexer.Haskell
import Quark.Window.Core
import Quark.Helpers
import Quark.Colors
import Quark.Types
import Quark.Cursor (orderTwo)

refresh :: Window -> IO ()
refresh (TextView w _ _)  = Curses.wRefresh w

cursorDirection :: Cursor -> Int -> Window -> Maybe Direction
cursorDirection (x, y) y0 (TextView _ (r, c) (rr, cc))
    | x < rr               = Just Up
    | x >= rr + r - 1      = Just Down
    | y < (cc - y0)        = Just Backward
    | y >= cc + c - y0 - 1 = Just Forward
    | otherwise            = Nothing

changeOffset' :: Cursor -> Int -> Window -> Window
changeOffset' crs ccOffset t = case cursorDirection crs ccOffset t of
    Nothing -> t
    Just d  -> changeOffset' crs ccOffset (changeOffset d t)

changeOffset :: Direction -> Window -> Window
changeOffset d (TextView w size (rr, cc))
    | d == Up       = TextView w size (max 0 (rr - rrStep), cc)
    | d == Down     = TextView w size (rr + rrStep, cc)
    | d == Backward = TextView w size (rr, max 0 (cc - ccStep))
    | d == Forward  = TextView w size (rr, cc + ccStep)
  where
    rrStep = 3
    ccStep = 5

-- TODO: add text overflow hints
printText :: Window -> (Cursor, Cursor) -> ByteString -> IO ()
printText w'@(TextView w (r, c) (rr, cc)) cursors text = do
    Curses.wclear w
    mapM_ (\(k, l, t, s) -> printTokenLine k l t s w') $
        zip4 [0..(r - 2)] lineNumbers tokens selections
  where
    n =  (length $ B.lines text) + if nlTail text then 1 else 0
    lnc = (length $ show n) + 1
    lineNumbers = map (padToLen lnc) (map (B.pack . show) $ [rr + 1..n]) ++ repeat ""
    tokens = drop rr $ tokenLines $ tokenizeHaskell text
    selections = map (selOnLine cursors) [rr + 0..n]

selOnLine :: (Cursor, Cursor) -> Row -> (Int, Int)
selOnLine (crs, sel) r
    | r < rIn || r > rOut = (-1, -1)
    | otherwise           = (cIn', cOut')
  where
    cIn' = if r == rIn then cIn else 0
    cOut' = if r == rOut then cOut else (-1)
    ((rIn, cIn), (rOut, cOut)) = orderTwo crs sel

printTokenLine :: Int -> ByteString -> [Token] -> (Col, Col) -> Window -> IO ()
printTokenLine k lNo tokens (cIn, cOut) w'@(TextView w (_, c) (_, c0)) = do
    Curses.wMove w k 0
    Curses.wAttrSet w (Curses.attr0, Curses.Pair lineNumberColor)
    Curses.wAddStr w $ B.unpack lNo
    mapM_ printToken $ selOnTokenLine adjustedSel $
        takeTL (c - B.length lNo) $ dropTL c0 tokens
  where
    adjustedSel = ( if cIn == (-1) then (-1) else max 0 (cIn - c0)
                  , if cOut == (-1) then (-1) else cOut - c0 )
    printToken (t, selected) = do
        setTokenColor t selected w'
        printToken' t w'

selOnTokenLine :: (Col, Col) -> [Token] -> [(Token, Bool)]
selOnTokenLine (-1, -1) ts = zip ts $ repeat False
selOnTokenLine (0, -1) ts  = zip ts $ repeat True
selOnTokenLine (cIn, cOut) (t:ts)
    | cIn >= n              = (t, False):selOnTokenLine nextSel ts
    | cIn == 0 && cOut >= n = (t, True):selOnTokenLine nextSel ts
    | cIn == 0              = (++) (zip (splitT t [cOut]) [True, False]) $
                                  selOnTokenLine nextSel ts
    | cOut >= n || cOut < 0 = (++) (zip (splitT t [cIn]) [False, True]) $
                                  selOnTokenLine nextSel ts
    | otherwise             = (++) (zip (splitT t [cIn, cOut]) [ False
                                                               , True
                                                               , False]) $
                                  selOnTokenLine nextSel ts
  where
    n = tokenLength t
    nextSel = ( max (cIn - n) 0
              , if cOut == (-1) then cOut else max (cOut - n) 0)
selOnTokenLine _ ts = zip ts $ repeat False

printToken' :: Token -> Window -> IO ()
printToken' t (TextView w _ _) = Curses.wAddStr w $ B.unpack $ tokenString t

setTokenColor :: Token -> Bool -> Window -> IO ()
setTokenColor t selected (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair $ haskellColors t + selOffset)
  where
    selOffset = if selected then 17 else 0