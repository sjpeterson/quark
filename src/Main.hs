module Main where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist
                        , doesDirectoryExist )

import qualified UI.HSCurses.Curses as Curses
-- import qualified UI.HSCurses.CursesHelper as CursesH
-- import Control.Exception (bracket_)

import Data.Char ( isPrint )

import Quark.Window
import Quark.Layout
import Quark.Buffer
import Quark.Flipper
import Quark.History
import Quark.Types
import Quark.Helpers
import Quark.UtilityBar

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
setTitle (TitleBar w (_, c)) title = do
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
    -- text <- if fileExists then readFile path else return "Nothing here"
    -- printText (primaryPane layout) text
    return layout

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
printText :: Window -> String -> IO ()
printText t@(TextView w (r, c) (rr, cc)) text = do
    mapM_ (\(k, l, s) ->
        printLine k l s t) $ zip3 [0..(r - 2)] lineNumbers textLines
  where
    n =  (length $ lines text) + if nlTail text then 1 else 0
    lnc = (length $ show n) + 1
    lineNumbers = map (padToLen lnc) (map show $ drop rr [1..n]) ++ repeat ""
    textLines =
        map ((padToLen (c - lnc)). drop cc) $ drop rr (lines text) ++ repeat ""

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

-- TODO: rewrite for style
initBuffer :: String -> IO (ExtendedBuffer)
initBuffer path = do
    fileExists <- doesFileExist path
    contents <- if fileExists then readFile path else return ""
    -- let title = if fileExists then path else "Untitled"
    return ((Buffer (fromString contents) (0, 0) (0, 0)), path, False)

initFlipper :: String -> IO (Flipper ExtendedBuffer)
initFlipper path = do
    extendedBuffer <- initBuffer path
    return (extendedBuffer, [], [])

-- TODO: clean this mess up!
promptSave :: Layout -> Flipper ExtendedBuffer -> IO ()
promptSave layout buffers = case active buffers of
    (b@(Buffer h _ _), path, False) ->
        do newPath <- promptString u "Save buffer to file:" path
           fileExists <- doesFileExist newPath
           let doConfirm = fileExists && path /= newPath
           dirExists <- doesDirectoryExist newPath
           let newBuffers = mapF (\(b, _, _) -> (b, newPath, False)) buffers
           if dirExists || newPath == ""
               then do debug u $ if newPath == ""
                                     then "Buffer was not saved"
                                     else path ++ " is a directory"
                       mainLoop layout buffers
               else if doConfirm
                        then confirmSave layout buffers newPath
                        else save layout newBuffers
    _                               ->
        do debug u "Can't save protected buffer"
           mainLoop layout buffers
  where
    u = utilityBar layout

confirmSave :: Layout -> Flipper ExtendedBuffer -> String -> IO ()
confirmSave layout buffers newPath = do
    overwrite <- promptChoice u promptText [ ('y', "Yes", True)
                                           , ('n', "No", False) ]
    if overwrite then save layout newBuffers
                 else do debug u "Buffer was not saved"
                         mainLoop layout buffers
  where
    u = utilityBar layout
    promptText = newPath ++ " already exists, overwrite?"
    newBuffers = mapF (\(b, _, _) -> (b, newPath, False)) buffers

save :: Layout -> Flipper ExtendedBuffer -> IO ()
save layout buffers = do
    writeFile path $ nlEnd $ toString h
    debug (utilityBar layout) $ path ++ " saved!"
    mainLoop layout $ mapF (\(b, _, _) -> (b, path, False)) buffers
  where
    ((Buffer h _ _), path, _) = active buffers
    nlEnd s  = if nlTail s then s else s ++ "\n"

-- TODO: consider moving to separate file
action :: (Buffer -> Buffer) -> Layout -> Flipper ExtendedBuffer -> IO ()
action a layout buffers = mainLoop layout $ mapF (mapXB a) buffers

handleKey :: Curses.Key -> Layout -> Flipper ExtendedBuffer -> IO ()
handleKey (Curses.KeyChar c) layout buffers
    | c == '\DC1' = end
    | c == '\DC3' = promptSave layout buffers
    | c == '\DEL' = action backspace layout buffers
    | c == '\SUB' = action undo layout buffers
    | c == '\EM'  = action redo layout buffers
    | c == '\DLE' = do let u = utilityBar layout
                       s <- promptString u "Hey man, how are you?" "default"
                       debug u $ "You are " ++ s ++ "!"
                       continue
    | c == '\SI'  = do let u = utilityBar layout
                       x <- promptChoice u "How many?" [ ('a', "a", 1)
                                                    , ('b', "b", 2) ]
                       debug u $ "You selected: " ++ (show x)
                       continue
    | c == '\r'   = action (input '\n') layout buffers
    | c == '\t'   = action (tab 4) layout buffers
    | isPrint c   = action (input c) layout buffers
    | otherwise   = continue
  where
    ((Buffer h _ _), _, _) = active buffers
    continue = mainLoop layout buffers
handleKey k layout buffers
    | k == Curses.KeyDC = action delete layout buffers
    | k == Curses.KeyLeft = action (moveCursor Backward) layout buffers
    | k == Curses.KeyRight = action (moveCursor Forward) layout buffers
    | k == Curses.KeyDown = action (moveCursor Down) layout buffers
    | k == Curses.KeyUp = action (moveCursor Up) layout buffers
    | k == Curses.KeyPPage = action (moveCursorN (r - 1) Up) layout buffers
    | k == Curses.KeyNPage = action (moveCursorN (r - 1) Down) layout buffers
    | k == Curses.KeyEnd = action endOfLine layout buffers
    | k == Curses.KeyHome = action startOfLine layout buffers
    | k == Curses.KeyUnknown 532 = action endOfFile layout buffers
    | k == Curses.KeyUnknown 537 = action startOfFile layout buffers
    | otherwise = mainLoop layout buffers
  where
    (TextView _ (r, _) _) = primaryPane layout

-- Start Curses and initialize colors
cursesMode :: IO ()
cursesMode = do
    Curses.echo False
    Curses.raw True     -- disable flow control characters
    Curses.nl False     -- maps Enter to C-m rather than C-j

start :: String -> IO ()
start path = do
    Curses.initScr
    hasColors <- Curses.hasColors
    if hasColors then do Curses.startColor
                         Curses.useDefaultColors
                         defineColors
                 else return ()
    --cursesMode
    Curses.resetParams
    Curses.wclear Curses.stdScr
    Curses.refresh
    layout <- initLayout path
    buffers <- initFlipper path
    mainLoop layout buffers

mainLoop :: Layout -> Flipper ExtendedBuffer -> IO ()
mainLoop layout buffers = do
    let lnOffset = lnWidth $ ebToString $ active buffers
    let crs = cursor $ (\(x, _, _) -> x ) $ active buffers
    let layout' = mapL (changeOffset' crs lnOffset) layout
    let w@(TextView _ _ (rr, cc)) = primaryPane layout'
    -- debug (utilityBar layout') $ show w
    printText w (ebToString $ active buffers)
    updateCursor w (rr, cc - lnOffset) crs
    setTitle (titleBar layout') $ show crs
    refresh w
    c <- Curses.getCh
    debug (utilityBar layout') $ show c
    handleKey c layout' buffers

end :: IO ()
end = Curses.endWin

main :: IO ()
main = do
    args <- getArgs
    let path = (\x -> if (length x) == 0 then "None" else head x) args
    start path
    -- or bracket pattern?