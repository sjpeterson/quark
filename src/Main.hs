module Main where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )

import qualified UI.HSCurses.Curses as Curses

import Quark.Window
import Quark.Layout

-- import qualified UI.HSCurses.CursesHelper as CursesH
-- import Control.Exception (bracket_)

-- neat shorthand for initializing foreground color f and background color b
-- as color pair n
defineColor :: Int -> Int -> Int -> IO ()
defineColor n f b =
  Curses.initPair (Curses.Pair n) (Curses.Color f) (Curses.Color b)

-- Initialize default color pairs from palette.
--
-- 1 through 16 are the following foreground colors on default background:
--
-- 1  - Red
-- 2  - Green
-- 3  - Brown/Dark yellow
-- 4  - Blue
-- 5  - Magenta
-- 6  - Cyan
-- 7  - Light gray
-- 8  - Dark gray
-- 9  - Light red
-- 10 - Light green
-- 11 - Yellow
-- 12 - Light blue
-- 13 - Light magenta
-- 14 - Light cyan
-- 15 - White
-- 16 - Black
--
-- 17 is for the title bar.
-- 18 is for line numbers

defineColors :: IO ()
defineColors = do
  mapM_ (\n -> defineColor n n (-1)) [1..16]
  -- defineColor 17 16 7
  defineColor 17 16 3
  defineColor 18 3 (-1)

setTitle :: Window -> String -> IO ()
setTitle (TitleBar w (r, c)) title = do
    Curses.attrSet Curses.attr0 (Curses.Pair 17)
    Curses.mvWAddStr w 0 0 (padMidToLen c leftText rightText)
    Curses.wRefresh w
  where
    leftText = " quark - " ++ title
    rightText = "0.0.1a "

initLayout :: String -> IO (Layout)
initLayout path = do
    layout <- defaultLayout
    fillBackground (titleBar layout) 17
    setTitle (titleBar layout) path
    fileExists <- doesFileExist path
    text <- if fileExists then readFile path else return "Nothing here"
    printText (primaryPane layout) text
    return layout

printText :: Window -> String -> IO ()
printText t@(TextView w (r, _) (rr, _)) text = do
    mapM_ (\(k, l, s) -> printLine k l s t) $ zip3 [1..r] lineNumbers textLines
    Curses.wMove w 1 lnc
    Curses.wRefresh w
  where
    n =  length $ lines text
    lnc = (length $ show n) + 1
    lineNumbers = map (padToLen lnc) (map show $ drop rr [1..n]) ++ repeat ""
    textLines = drop rr (lines text) ++ repeat ""

printLine :: Int -> String -> String -> Window -> IO ()
printLine k lineNumber text (TextView w (_, c) _) = do
    Curses.wMove w k 0
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 18)
    Curses.wAddStr w lineNumber
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 0)
    Curses.wAddStr w $ take (c - length lineNumber) text

fillBackground :: Window -> Int -> IO ()
fillBackground (TitleBar cTitleBar (h, w)) colorId = do
    Curses.wAttrSet cTitleBar (Curses.attr0, Curses.Pair colorId)
    Curses.mvWAddStr cTitleBar 0 0 (take w $ repeat ' ')
    Curses.wMove cTitleBar 1 0
    Curses.wRefresh cTitleBar

padToLen :: Int -> [Char] -> [Char]
padToLen k a
  | k <= length a = a
  | otherwise     = padToLen k $ a ++ " "

updateTitleBar :: Curses.Window -> Int -> String -> IO ()
updateTitleBar win maxCols path = do
    Curses.attrSet Curses.attr0 (Curses.Pair 17)
    Curses.mvWAddStr win 0 0 (padMidToLen maxCols leftText rightText)
    Curses.wRefresh win
  where
    leftText = " quark - " ++ path
    rightText = "0.0.1a "

padMidToLen :: Int -> [Char] -> [Char] -> [Char]
padMidToLen k a0 a1
  | k == (length a0 + length a1) = a0 ++ a1
  | otherwise                    = padMidToLen k (a0 ++ " ") a1

-- Start Curses and initialize colors
start :: String -> IO ()
start path = do
  Curses.initScr
  hasColors <- Curses.hasColors
  if hasColors
    then do
      Curses.startColor
      Curses.useDefaultColors
      defineColors
      return ()
    else
      return ()
  Curses.echo False
  Curses.wclear Curses.stdScr
  Curses.refresh
  layout <- initLayout path
  mainLoop layout

mainLoop layout = do
    c <- Curses.getch
    if Curses.decodeKey c == Curses.KeyChar 'q' then end
                                                else mainLoop layout
end :: IO ()
end = Curses.endWin

main :: IO ()
main = do
    args <- getArgs
    let path = (\x -> if (length x) == 0 then "None" else head x) args
    start path
  -- or bracket pattern?