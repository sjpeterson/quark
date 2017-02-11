{-# LANGUAGE OverloadedStrings #-}

--------
--
-- Module:      Quark.UtilityBar
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Module for functions related to the utility bar
--
--------

module Quark.UtilityBar ( promptString
                        , promptChoice
                        , debug ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char ( isPrint
                 , toUpper )

import qualified UI.HSCurses.Curses as Curses

import Quark.Window ( Window ( UtilityBar )
                    , updateCursor )
import Quark.Helpers ( padToLen
                     , padToLen'
                     , (~~))
import Quark.Buffer ( Buffer ( Buffer )
                    , endOfLine
                    , startOfLine
                    , input
                    , delete
                    , backspace
                    , moveCursor
                    , undo
                    , redo )
import Quark.Types
import Quark.History ( fromString
                     , toString )


prompt :: Int -> Window -> ByteString -> IO ()
prompt r (UtilityBar w (_, c)) text = do
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 1)
    Curses.mvWAddStr w r 0 $ B.unpack $ padToLen (c - 1) text
    Curses.wRefresh w

debug = prompt 0

clear :: Window -> IO ()
clear u = do
    prompt 0 u ""
    prompt 1 u ""

promptString :: Window -> ByteString -> ByteString -> IO (String)
promptString u s def = do
    prompt 0 u s
    let b = endOfLine True $ Buffer (fromString def) (0, 0) (0, 0)
    s' <- cGetLine u b
    clear u
    return s'

promptChoice :: (Show a) => Window -> ByteString -> [Option a] -> IO (a)
promptChoice u s options = do
    prompt 0 u s
    x <- cGetOption u options
    clear u
    return x

cGetLine :: Window -> Buffer -> IO (String)
cGetLine u@(UtilityBar w _) buffer@(Buffer h cursor _) = do
    prompt 1 u $ toString h
    updateCursor u (1, 0) cursor
    Curses.wRefresh w
    c <- Curses.getCh
    handleKey c u buffer

cGetOption :: (Show a) => Window -> [Option a] -> IO (a)
cGetOption u@(UtilityBar w _) options = do
    let n = maximum $ map optionLength options
    Curses.wMove w 1 0
    mapM (printOption w n) options
    Curses.wRefresh w
    c' <- Curses.getCh
    case checkOption c' options of Nothing -> cGetOption u options
                                   Just x  -> return x
  where
    optionLength = \(c, s, _) -> length (translateChar c) + B.length s + 1

checkOption :: Curses.Key -> [Option a] -> Maybe a
checkOption _ [] = Nothing
checkOption k@(Curses.KeyChar c) ((c', _, x):xs)
    | toUpper c == toUpper c' = Just x
    | otherwise               = checkOption k xs

printOption :: (Show a) => Curses.Window -> Int -> Option a -> IO ()
printOption w n (c, s, _) = do
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 0)
    Curses.wAddStr w sc
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 1)
    Curses.wAddStr w $ B.unpack (padToLen' (n - length sc) s) ++ " "
  where
    sc = translateChar c

-- expand with '\ETX' to "^C" and such as needed
translateChar :: Char -> String
translateChar '\ESC' = "ESC"
translateChar c = [toUpper c]

-- TODO: This repeats a subset of what's in Main.hs - overload?
handleKey :: Curses.Key -> Window -> Buffer -> IO (String)
handleKey (Curses.KeyChar c) u buffer@(Buffer h _ _)
    | c == '\ESC' = return $ "\ESC"
    | c == '\r'   = return $ B.unpack $ toString h
    | c == '\DEL' = cGetLine u $ backspace buffer
    | c == '\SUB' = cGetLine u $ undo buffer
    | c == '\EM'  = cGetLine u $ redo buffer
    | isPrint c   = cGetLine u $ input c buffer
    | otherwise   = cGetLine u $ buffer
handleKey k u buffer
    | k == Curses.KeyDC = cGetLine u $ delete buffer
    | k == Curses.KeyLeft = cGetLine u $ (moveCursor Backward) buffer
    | k == Curses.KeyRight = cGetLine u $ (moveCursor Forward) buffer
    | k == Curses.KeyEnd = cGetLine u $ endOfLine True buffer
    | k == Curses.KeyHome = cGetLine u $ startOfLine True buffer
    | k == Curses.KeyUnknown 532 = cGetLine u $ endOfLine True buffer
    | k == Curses.KeyUnknown 537 = cGetLine u $ startOfLine True buffer
    | otherwise = cGetLine u $ buffer
