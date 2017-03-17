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

module Quark.Window.TextView ( printText
                             , updateOffset ) where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U

import Quark.Frontend.HSCurses ( Window ( TextView )
                               , setTextColor
                               , move
                               , addString
                               , clear'
                               , refresh )

import Quark.Lexer.Core ( tokenLength
                        , tokenString
                        , tokenLines
                        , splitT
                        , dropTL
                        , takeTL )
import Quark.Lexer.Language
import Quark.Helpers
import Quark.Colors
import Quark.Types
import Quark.Cursor (orderTwo)

printText :: Language -> Window -> (Cursor, Cursor) -> Bool -> [[Token]]
          -> IO ()
printText language w@(TextView _ (r, c) (rr, cc)) (crs, sel) m tokenLines' = do
    clear' w lnc
    mapM_ (\(k, l, t, s) -> printTokenLine language k l t s w) $
        zip4 [0..(r - 2)] lineNumbers tokens selections
    refresh w
  where
    n = length tokenLines'
    lnc = (length $ show n) + 1
    lineNumbers =
        map (padToLen lnc) (map (U.fromString . show) $
            [rr + 1..n]) ++ repeat ""
    tokens = drop rr tokenLines'
    selections = map (selOnLine (crs, sel')) [rr + 0..n]
    sel' = if m && crs == sel then (rSel, cSel + 1) else sel
    (rSel, cSel) = sel


updateOffset :: Cursor -> Int -> Window -> Window
updateOffset crs ccOffset t = case cursorDirection crs ccOffset t of
    Nothing -> t
    Just d  -> updateOffset crs ccOffset (changeOffset d t)

cursorDirection :: Cursor -> Int -> Window -> Maybe Direction
cursorDirection (x, y) y0 (TextView _ (r, c) (rr, cc))
    | x < rr               = Just Up
    | x >= rr + r - 1      = Just Down
    | y < (cc - y0)        = Just Backward
    | y >= cc + c - y0 - 1 = Just Forward
    | otherwise            = Nothing

changeOffset :: Direction -> Window -> Window
changeOffset d (TextView w size (rr, cc))
    | d == Up       = TextView w size (max 0 (rr - rrStep), cc)
    | d == Down     = TextView w size (rr + rrStep, cc)
    | d == Backward = TextView w size (rr, max 0 (cc - ccStep))
    | d == Forward  = TextView w size (rr, cc + ccStep)
  where
    rrStep = 3
    ccStep = 5

printTokenLine :: Language
               -> Int
               -> ByteString
               -> [Token]
               -> (Col, Col)
               -> Window
               -> IO ()
printTokenLine language k lNo tokens (cIn, cOut) w' = do
    move w' k 0
    setTextColor w' lineNumberPair
    addString w' $ U.toString lNo
    mapM_ printToken $ selOnTokenLine adjustedSel $
        takeTL (c - U.length lNo) $ dropTL c0 tokens
  where
    (TextView w (_, c) (_, c0)) = w'
    adjustedSel = ( if cIn == (-1) then (-1) else max 0 (cIn - c0)
                  , if cOut == (-1) then (-1) else cOut - c0 )
    printToken (t, selected) = do
        setTokenColor language t selected w'
        printToken' t w'

selOnLine :: (Cursor, Cursor) -> Row -> (Int, Int)
selOnLine (crs, sel) r
    | r < rIn || r > rOut = (-1, -1)
    | otherwise           = (cIn', cOut')
  where
    cIn' = if r == rIn then cIn else 0
    cOut' = if r == rOut then cOut else (-1)
    ((rIn, cIn), (rOut, cOut)) = orderTwo crs sel

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
printToken' t w = addString w $ U.toString $ tokenString t

setTokenColor :: Language -> Token -> Bool -> Window -> IO ()
setTokenColor language t selected w =
    setTextColor w (colorize language t, tokenBg)
  where
    tokenBg = case selected of False -> defaultBg
                               True  -> selectionColor