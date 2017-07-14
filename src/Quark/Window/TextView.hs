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

import qualified Data.Text as T

import Quark.Frontend.HSCurses ( Window ( TextView )
                               , setTextColor
                               , move
                               , addString
                               , clear'
                               , refresh
                               , showCursor )

import Quark.Lexer.Core ( tokenLength
                        , tokenString
                        , tokenLines
                        , hintTabs
                        , splitT
                        , dropTL
                        , takeTL )
import Quark.Lexer.Language
import Quark.Helpers
import Quark.Colors
import Quark.Types

printText :: Language
          -> Window
          -> (Cursor, Cursor)
          -> [Cursor]
          -> Bool
          -> [[Token]]
          -> IO ()
printText language w (crs, sel) hs m tokenLines' = do
    clear' w lnc
    mapM_ (\(k, l, t, s, h) -> printTokenLine language k l t s h w) $
        zip5 [0..(r - 2)] lineNumbers tokens selections highlights
  where
    (TextView _ (r, c) (rr, cc)) = w
    n = length tokenLines'
    lnc = (length $ show n) + 1
    lineNumbers =
        map (padToLen lnc) (map (T.pack . show) $
            [rr + 1..n]) ++ repeat ""
    tokens = map hintTabs $ drop rr tokenLines'
    selections = map (selOnLine (crs, sel')) [rr + 0..n]
    highlights = map (hlOnLine hs) [rr + 0..n]
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
               -> T.Text
               -> [Token]
               -> (Col, Col)
               -> [Col]
               -> Window
               -> IO ()
printTokenLine language k lNo tokens (cIn, cOut) hlCols w' = do
    move w' k 0
    setTextColor w' lineNumberPair
    addString w' $ T.unpack lNo
    mapM_ printToken $ hlOnTokenLine hlCols $ selOnTokenLine adjustedSel $
        takeTL (c - T.length lNo) $ dropTL c0 tokens
  where
    (TextView w (_, c) (_, c0)) = w'
    adjustedSel = ( if cIn == (-1) then (-1) else max 0 (cIn - c0)
                  , if cOut == (-1) then (-1) else cOut - c0 )
    printToken (t, tokenState) = do
        setTokenColor language t tokenState w'
        printToken' t w'

selOnLine :: (Cursor, Cursor) -> Row -> (Col, Col)
selOnLine (crs, sel) r
    | r < rIn || r > rOut = (-1, -1)
    | otherwise           = (cIn', cOut')
  where
    cIn' = if r == rIn then cIn else 0
    cOut' = if r == rOut then cOut else (-1)
    ((rIn, cIn), (rOut, cOut)) = orderTwo crs sel

selOnTokenLine :: (Col, Col) -> [Token] -> [(Token, TokenState)]
selOnTokenLine (-1, -1) ts = zip ts $ repeat DefaultState
selOnTokenLine (0, -1) ts  = zip ts $ repeat Selected
selOnTokenLine (cIn, cOut) (t:ts)
    | cIn >= n              = (t, DefaultState):selOnTokenLine nextSel ts
    | cIn == 0 && cOut >= n = (t, Selected):selOnTokenLine nextSel ts
    | cIn == 0              =
        (++) (zip (splitT t [cOut]) [Selected, DefaultState]) $
            selOnTokenLine nextSel ts
    | cOut >= n || cOut < 0 =
        (++) (zip (splitT t [cIn]) [DefaultState, Selected]) $
            selOnTokenLine nextSel ts
    | otherwise             =
        (++) (zip (splitT t [cIn, cOut]) [ DefaultState
                                         , Selected
                                         , DefaultState]) $
            selOnTokenLine nextSel ts
  where
    n = tokenLength t
    nextSel = ( max (cIn - n) 0
              , if cOut == (-1) then cOut else max (cOut - n) 0)
selOnTokenLine _ ts = zip ts $ repeat DefaultState

hlOnLine :: [Cursor] -> Row -> [Col]
hlOnLine highlights r = [c' | (r', c') <- highlights, r' == r]

hlOnTokenLine :: [Col] -> [(Token, TokenState)] -> [(Token, TokenState)]
hlOnTokenLine _ [] = []
hlOnTokenLine [] ts = ts
hlOnTokenLine hlCols (t@(t', state):ts) =
    newT:(hlOnTokenLine (map (\x -> x - n) hlCols) ts)
  where
    newT = case t' of
        Unclassified _ -> t
        _              -> if state == DefaultState && (elem 0 hlCols)
                              then (t', Highlighted)
                              else t
    n = tokenLength t'

printToken' :: Token -> Window -> IO ()
printToken' t w = addString w $ T.unpack $ tokenString t

setTokenColor :: Language -> Token -> TokenState -> Window -> IO ()
setTokenColor _ (Tabs _) tokenState w =
    setTextColor w (hintColor, tokenBg tokenState)
setTokenColor language t tokenState w =
    setTextColor w (colorize language t, tokenBg tokenState)

tokenBg :: TokenState -> Int
tokenBg DefaultState = defaultBg
tokenBg Selected     = selectionColor
tokenBg Highlighted  = highlightColor