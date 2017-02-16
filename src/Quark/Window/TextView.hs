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

-- TODO: show hints of overflow
printText :: Window -> (Index, Index) -> ByteString -> IO ()
printText t@(TextView w (r, c) (rr, cc)) q text = do
    mapM_ (\(k, l, s) ->
        printLine k l s t) $ zip3 [0..(r - 2)] lineNumbers textLines
  where
    n =  (length $ B.lines text) + if nlTail text then 1 else 0
    lnc = (length $ show n) + 1
    lineNumbers = map (padToLen lnc) (map (B.pack . show) $ drop rr [1..n]) ++ repeat ""
    textLines =
        map ((padToLenSL (c - lnc)) . dropSL cc) $
            drop rr (selectionLines $ splitAt2 q text) ++ repeat [("", False)]

printLine :: Int -> ByteString -> [(ByteString, Bool)] -> Window -> IO ()
printLine k lineNumber text (TextView w (_, c) _) = do
    Curses.wMove w k 0
    Curses.wAttrSet w (Curses.attr0, Curses.Pair lineNumberColor)
    Curses.wAddStr w $ B.unpack lineNumber
    mapM_ printSection text -- $ [(take (c - length lineNumber) text, False)]
  where
    printSection (s, selected) = do
        Curses.wAttrSet w $ if selected then (Curses.attr0, Curses.Pair 19)
                                        else (Curses.attr0, Curses.Pair 0)
        Curses.wAddStr w $ B.unpack s

printText' :: Window -> (Index, Index) -> ByteString -> IO ()
printText' t@(TextView w (r, c) (rr, cc)) q text = do
    Curses.wclear w
    mapM_ (\(k, l, s) -> printTokenLine k l s t) $
        zip3 [0..(r - 2)] lineNumbers tokens
  where
    n =  (length $ B.lines text) + if nlTail text then 1 else 0
    lnc = (length $ show n) + 1
    lineNumbers = map (padToLen lnc) (map (B.pack . show) $ [rr + 1..n]) ++ repeat ""
    tokens = drop rr $ tokenLines $ tokenizeHaskell text

printTokenLine :: Int -> ByteString -> [Token] -> Window -> IO ()
printTokenLine k lineNumber tokens w'@(TextView w (_, c) (_, c0)) = do
    Curses.wMove w k 0
    Curses.wAttrSet w (Curses.attr0, Curses.Pair lineNumberColor)
    Curses.wAddStr w $ B.unpack lineNumber
    mapM_ printTokens $ takeTL (c - B.length lineNumber) $ dropTL c0 tokens
  where
    printTokens t = do
        setTokenColor t w'
        printToken t w'

printToken :: Token -> Window -> IO ()
printToken t (TextView w _ _) = Curses.wAddStr w $ B.unpack $ tokenString t

setTokenColor :: Token -> Window -> IO ()
setTokenColor t (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair $ haskellColors t)