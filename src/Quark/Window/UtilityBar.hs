{-# LANGUAGE OverloadedStrings #-}

--------
--
-- Module:      Quark.Window.UtilityBar
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

module Quark.Window.UtilityBar ( promptString
                               , promptChoice
                               , debug ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char ( isPrint
                 , toUpper )

import Quark.Frontend.HSCurses ( Window ( UtilityBar )
                               , updateCursor
                               , setTextColor
                               , move
                               , addString
                               , mvAddString
                               , refresh
                               , getKey )

import Quark.Helpers ( padToLen
                     , padToLen'
                     , (~~))
import Quark.Buffer ( Buffer ( Buffer )
                    , endOfLine
                    , startOfLine
                    , input
                    , paste
                    , delete
                    , backspace
                    , moveCursor
                    , undo
                    , redo )
import Quark.Types
import Quark.History ( fromString
                     , toString )
import Quark.Colors
import Quark.IOHelpers ( autoComplete )


prompt :: Int -> Window -> ByteString -> IO ()
prompt r w@(UtilityBar _ (_, c)) text = do
    setTextColor w (red, defaultBg)
    mvAddString w r 0 $ B.unpack $ padToLen (c - 1) text
    refresh w

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
cGetLine w buffer@(Buffer h cursor _) = do
    prompt 1 w $ toString h
    updateCursor w (1, 0) cursor
    refresh w
    k <- getKey
    handleKey k w buffer

cGetOption :: (Show a) => Window -> [Option a] -> IO (a)
cGetOption w options = do
    let n = maximum $ map optionLength options
    move w 1 0
    mapM (printOption w n) options
    refresh w
    k <- getKey
    case checkOption k options of Nothing -> cGetOption w options
                                  Just x  -> return x
  where
    optionLength = \(c, s, _) -> length (translateChar c) + B.length s + 1

checkOption :: Key -> [Option a] -> Maybe a
checkOption _ [] = Nothing
checkOption k@(CharKey c) ((c', _, x):xs)
    | toUpper c == toUpper c' = Just x
    | otherwise               = checkOption k xs
checkOption _ _ = Nothing

printOption :: (Show a) => Window -> Int -> Option a -> IO ()
printOption w n (c, s, _) = do
    setTextColor w (defaultColor, defaultBg)
    addString w sc
    setTextColor w (red, defaultBg)
    addString w $ B.unpack (padToLen' (n - length sc) s) ++ " "
  where
    sc = translateChar c

-- expand with '\ETX' to "^C" and such as needed
translateChar :: Char -> String
translateChar '\ESC' = "ESC"
translateChar c = [toUpper c]

promptAutoComplete :: Window -> Buffer -> Int -> IO (String)
promptAutoComplete w buffer@(Buffer h cursor _) n = do
    s <- autoComplete n $ B.unpack $ toString h
    prompt 1 w $ (toString h) ~~ (B.pack s)
    updateCursor w (1, length s) cursor
    refresh w
    k <- getKey
    if k == SpecialKey "Tab"
        then promptAutoComplete w buffer (n + 1)
        else handleKey k w (paste (B.pack s) buffer)

handleKey :: Key -> Window -> Buffer -> IO (String)
handleKey (CharKey c) w buffer = cGetLine w $ input c buffer
handleKey k w buffer@(Buffer h _ _)
    | k == SpecialKey "Esc"        = return "\ESC"
    | k == SpecialKey "Return"     = return $ B.unpack $ toString h
    | k == SpecialKey "Backspace"  = action backspace
    | k == CtrlKey 'z'             = action undo
    | k == CtrlKey 'y'             = action redo
    | k == SpecialKey "Tab"        = promptAutoComplete w buffer 0
    | k == SpecialKey "Delete"     = action delete
    | k == SpecialKey "Left"       = action $ moveCursor Backward
    | k == SpecialKey "Right"      = action $ moveCursor Forward
    | k == SpecialKey "End"        = action $ endOfLine True
    | k == SpecialKey "Home"       = action $ startOfLine True
    | k == SpecialKey "Shift-Home" = action $ endOfLine False
    | k == SpecialKey "Shift-End"  = action $ startOfLine False
    | otherwise = cGetLine w buffer
  where
    action a = cGetLine w $ a buffer
