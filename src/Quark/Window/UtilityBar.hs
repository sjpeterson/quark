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

import qualified Data.Text as T

import Data.Char ( isPrint
                 , toUpper )

import Quark.Frontend.HSCurses ( Window ( UtilityBar )
                               , windowSize
                               , updateCursor
                               , hideCursor
                               , showCursor
                               , setTextColor
                               , move
                               , addString
                               , mvAddString
                               , refresh
                               , getKey
                               , clear )

import Quark.Helpers ( padToLen
                     , fixToLen'
                     , (~~))
import Quark.Buffer ( Buffer ( Buffer )
                    , endOfLine
                    , startOfLine
                    , input
                    , insert
                    , paste
                    , delete
                    , backspace
                    , moveCursor
                    , undo
                    , redo )
import Quark.Types
import Quark.History ( emptyEditHistory )
import Quark.Colors
import Quark.IOHelpers ( autoComplete )


prompt :: Int -> Window -> T.Text -> IO ()
prompt r w@(UtilityBar _ (_, c)) text = do
    setTextColor w (red, defaultBg)
    mvAddString w r 0 $ T.unpack $ padToLen (c - 1) text
    refresh w

debug :: Window -> T.Text -> IO ()
debug = prompt 0

promptString :: Window -> T.Text -> T.Text -> IO (T.Text)
promptString u s def = do
    prompt 0 u s
    let b = endOfLine True $ Buffer def emptyEditHistory (0, 0) (0, 0)
    s' <- cGetLine u b
    clear u
    return s'

promptChoice :: Window -> T.Text -> [Option a] -> IO (a)
promptChoice u s options = do
    hideCursor
    prompt 0 u s
    x <- cGetOption u options
    clear u
    refresh u
    showCursor
    return x

cGetLine :: Window -> Buffer -> IO (T.Text)
cGetLine w buffer@(Buffer s h cursor@(_, c) _) = do
    prompt 1 w $ (T.take (n - 1) . T.drop m) s
    updateCursor w (1, -m) cursor
    refresh w
    k <- getKey
    handleKey k w buffer
  where
    (_, n) = windowSize w
    n_s = T.length s
    m = if n_s < n
            then 0
            else min (n_s - n + 1) (max 0 $ c - (div (2*n) 3))

cGetOption :: Window -> [Option a] -> IO (a)
cGetOption w@(UtilityBar _ (_, c)) options = do
    let n = min nMax $ maximum $ map optionLength options
    move w 1 0
    mapM (printOption w n) options
    refresh w
    k <- getKey
    case checkOption k options of Nothing -> cGetOption w options
                                  Just x  -> return x
  where
    nMax = (div c $ length options) - 1
    optionLength = \(c, s, _) -> length (translateChar c) + T.length s + 1

checkOption :: Key -> [Option a] -> Maybe a
checkOption _ [] = Nothing
checkOption k@(SpecialKey "Esc") ((c', _, x):xs)
    | c' == '\ESC' = Just x
    | otherwise    = checkOption k xs
checkOption k@(CharKey c) ((c', _, x):xs)
    | toUpper c == toUpper c' = Just x
    | otherwise               = checkOption k xs
checkOption _ _ = Nothing

printOption :: Window -> Int -> Option a -> IO ()
printOption w n (c, s, _) = do
    setTextColor w (defaultColor, defaultBg)
    addString w sc
    setTextColor w (red, defaultBg)
    addString w $ T.unpack (fixToLen' (n - length sc) s) ++ " "
  where
    sc = translateChar c

-- expand with '\ETX' to "^C" and such as needed
translateChar :: Char -> String
translateChar '\ESC' = "ESC"
translateChar c = [toUpper c]

promptAutoComplete :: Window -> Buffer -> Int -> IO (T.Text)
promptAutoComplete w buffer@(Buffer s' h cursor _) n = do
    s <- autoComplete n $ T.unpack $ s'
    prompt 1 w $ s' ~~ (T.pack s)
    updateCursor w (1, length s) cursor
    refresh w
    k <- getKey
    if k == SpecialKey "Tab"
        then promptAutoComplete w buffer (n + 1)
        else handleKey k w (paste (T.pack s) buffer)

handleKey :: Key -> Window -> Buffer -> IO (T.Text)
handleKey (CharKey c) w buffer = cGetLine w $ input c False buffer
handleKey (WideCharKey s) w buffer =
    cGetLine w $ insert (T.pack s) False True buffer
handleKey k w buffer@(Buffer s h _ _)
    | k == SpecialKey "Esc"        = return "\ESC"
    | k == SpecialKey "Return"     = return s
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
