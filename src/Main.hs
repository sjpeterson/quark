{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist
                        , doesDirectoryExist )
import System.Clipboard

import qualified UI.HSCurses.Curses as Curses
-- import qualified UI.HSCurses.CursesHelper as CursesH
-- import Control.Exception (bracket_)

import Data.Char ( isPrint )
import Data.List ( findIndices )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Quark.Window
import Quark.Layout
import Quark.Buffer
import Quark.Flipper
import Quark.History
import Quark.Types
import Quark.Helpers
import Quark.UtilityBar
import Quark.Lexer.Core
import Quark.Lexer.Haskell

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
-- 17 is for the title bar
-- 18 is for line numbers
-- 19 is for selections

defineColors :: IO ()
defineColors = do
  mapM_ (\n -> defineColor n n (-1)) [1..16]
  -- defineColor 17 16 7
  defineColor 17 16 3
  defineColor 18 7 (-1)
  defineColor 19 (-1) 8

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

initBuffer :: String -> IO (ExtendedBuffer)
initBuffer path = do
    fileExists <- doesFileExist path
    contents <- if fileExists then B.readFile path else return ""
    return ((Buffer (fromString contents) (0, 0) (0, 0)), path, False)

initFlipper :: String -> IO (Flipper ExtendedBuffer)
initFlipper path = do
    extendedBuffer <- initBuffer path
    return (extendedBuffer, [], [])

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
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 18)
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
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 18)
    Curses.wAddStr w $ B.unpack lineNumber
    mapM_ printTokens $ takeTL (c - B.length lineNumber) $ dropTL c0 tokens
  where
    printTokens t = do
        setTokenColor t w'
        printToken t w'

printToken :: Token -> Window -> IO ()
printToken t (TextView w _ _) = Curses.wAddStr w $ B.unpack $ tokenString t

setTokenColor :: Token -> Window -> IO ()
setTokenColor (ReservedIdent _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 3)
setTokenColor (StringLiteral _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 2)
setTokenColor (Comment _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 7)
setTokenColor (Pragma _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 11)
setTokenColor (CharLiteral _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 14)
setTokenColor (NumLiteral _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 12)
setTokenColor (Operator _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 3)
setTokenColor (Separator _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 3)
setTokenColor (TypeIdent _) (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 5)
setTokenColor _ (TextView w _ _) =
    Curses.wAttrSet w (Curses.attr0, Curses.Pair 0)

fillBackground :: Window -> Int -> IO ()
fillBackground (TitleBar cTitleBar (h, w)) colorId = do
    Curses.wAttrSet cTitleBar (Curses.attr0, Curses.Pair colorId)
    Curses.mvWAddStr cTitleBar 0 0 (take w $ repeat ' ')
    Curses.wMove cTitleBar 1 0
    Curses.wRefresh cTitleBar

saveAndQuit :: [Int] -> Layout -> Flipper ExtendedBuffer -> IO ()
saveAndQuit [] _ _ = end
saveAndQuit (x:xs) layout buffers = do
    (newBuffers, cancel) <- chooseSave layout $ flipTo x buffers
    if cancel then mainLoop layout newBuffers
              else saveAndQuit xs layout newBuffers

chooseSave :: Layout
           -> Flipper ExtendedBuffer
           -> IO (Flipper ExtendedBuffer, Bool)
chooseSave layout buffers = do
    save <- promptChoice u promptText [ ('y', "Yes", Just True)
                                      , ('n', "No", Just False)
                                      , ('\ESC', "Cancel", Nothing) ]
    chooseSave' save layout buffers
  where
    u = utilityBar layout
    promptText = B.pack $ "Save changes to " ++ pathOrUntitled ++ "?"
    pathOrUntitled = if path == "" then "Untitled" else path
    (_, path, _) = active buffers

chooseSave' :: Maybe Bool
            -> Layout
            -> Flipper ExtendedBuffer
            -> IO (Flipper ExtendedBuffer, Bool)
chooseSave' (Just True) layout buffers = do
    (newBuffers, canceled) <- if path == "" then promptSave layout buffers
                                            else writeFXB path buffers
    if canceled
        then chooseSave layout buffers
        else if newBuffers == buffers
                 then chooseSave' (Just True) layout buffers
                 else return (newBuffers, False)
  where
    (_, path, _) = active buffers
chooseSave' (Just False) _ buffers = return (buffers, False)
chooseSave' Nothing _ buffers = return (buffers, True)

promptSave :: Layout
           -> Flipper ExtendedBuffer
           -> IO (Flipper ExtendedBuffer, Bool)
promptSave layout buffers = case active buffers of
    (b@(Buffer h _ _), path, False) ->
        do newPath <- promptString u (B.pack "Save buffer to file:") $ B.pack path
           fileExists <- doesFileExist newPath
           let doConfirm = fileExists && path /= newPath
           dirExists <- doesDirectoryExist newPath
           if dirExists || newPath == ""
               then do debug u $ B.pack $ if newPath == ""
                                              then "Buffer was not saved"
                                              else path ++ " is a directory"
                       return (buffers, False)
               else if doConfirm
                        then confirmSave newPath layout buffers
                        else if newPath == "\ESC"
                                 then return (buffers, True)
                                 else do debug u $ B.pack $ "Saved " ++ newPath
                                         writeFXB newPath buffers
    _                               ->
        do debug u "Can't save protected buffer"
           return (buffers, True)
  where
    u = utilityBar layout

confirmSave :: FilePath
            -> Layout
            -> Flipper ExtendedBuffer
            -> IO (Flipper ExtendedBuffer, Bool)
confirmSave newPath layout buffers = do
    overwrite <- promptChoice u promptText [ ('y', "Yes", True)
                                           , ('n', "No", False) ]
    if overwrite then do debug u $ "Saved " ~~ (B.pack newPath)
                         writeFXB newPath buffers
                 else do debug u "Buffer was not saved"
                         return (buffers, False)
  where
    u = utilityBar layout
    promptText = (B.pack newPath) ~~ " already exists, overwrite?"

writeFXB :: FilePath
         -> Flipper ExtendedBuffer
         -> IO (Flipper ExtendedBuffer, Bool)
writeFXB path buffers = do
    newXB <- writeXB path xb
    return ((newXB, x, y), False)
  where
    (xb, x, y) = buffers

writeXB :: FilePath -> ExtendedBuffer -> IO ExtendedBuffer
writeXB path (b, _, False) = do
    newB <- writeBuffer path b
    return $ (newB, path, False)

writeBuffer :: FilePath -> Buffer -> IO Buffer
writeBuffer path (Buffer h@(n, p, f) c s) = do
    B.writeFile path $ nlEnd $ toString h -- no real need to unpack
    return $ Buffer (0, p, f) c s
  where
    nlEnd s  = if nlTail s then s else s ~~ "\n"

-- TODO: consider moving to separate file
action :: (Buffer -> Buffer) -> Layout -> Flipper ExtendedBuffer -> IO ()
action a layout buffers = mainLoop layout $ mapF (mapXB a) buffers

handleKey :: Curses.Key -> Layout -> Flipper ExtendedBuffer -> IO ()
handleKey (Curses.KeyChar c) layout buffers
    | c == '\DC1' = saveAndQuit unsavedIndices layout buffers
    | c == '\DC3' = do (newBuffers, _) <- promptSave layout buffers
                       mainLoop layout newBuffers
    | c == '\DEL' = action backspace layout buffers
    | c == '\SUB' = action undo layout buffers
    | c == '\EM'  = action redo layout buffers
    | c == '\CAN' = do setClipboardString $ B.unpack $ selection $ condense $
                           active buffers
                       action delete layout buffers
    | c == '\ETX' = do setClipboardString $ B.unpack $ selection $ condense $
                           active buffers
                       mainLoop layout buffers
    | c == '\SYN' = do s <- getClipboardString
                       case s of Nothing -> mainLoop layout buffers
                                 Just s' -> action (paste $ B.pack s') layout buffers
    | c == '\SOH' = action selectAll layout buffers
    | c == '\r'   = action nlAutoIndent layout buffers
    | c == '\t'   = action (tab 4) layout buffers
    | isPrint c   = action (input c) layout buffers
    | otherwise   = continue
  where
    unsavedIndices = findIndices unsavedXB $ toList buffers
    ((Buffer h _ _), _, _) = active buffers
    continue = mainLoop layout buffers
handleKey k layout buffers
    | k == Curses.KeyDC = action delete layout buffers
    | k == Curses.KeyLeft = action (moveCursor Backward) layout buffers
    | k == Curses.KeyRight = action (moveCursor Forward) layout buffers
    | k == Curses.KeyDown = action (moveCursor Down) layout buffers
    | k == Curses.KeyUp = action (moveCursor Up) layout buffers
    | k == Curses.KeySLeft = action (selectMoveCursor Backward) layout buffers
    | k == Curses.KeySRight = action (selectMoveCursor Forward) layout buffers
    | k == Curses.KeySNext = action (selectMoveCursor Down) layout buffers
    | k == Curses.KeySPrevious = action (selectMoveCursor Up) layout buffers
    | k == Curses.KeyPPage = action (moveCursorN (r - 1) Up) layout buffers
    | k == Curses.KeyNPage = action (moveCursorN (r - 1) Down) layout buffers
    | k == Curses.KeyEnd = action (endOfLine True) layout buffers
    | k == Curses.KeyHome = action (startOfLine True) layout buffers
    | k == Curses.KeySEnd = action (endOfLine False) layout buffers
    | k == Curses.KeySHome = action (startOfLine False) layout buffers
    | k == Curses.KeyUnknown 532 = action (endOfFile True) layout buffers
    | k == Curses.KeyUnknown 537 = action (startOfFile True) layout buffers
    | k == Curses.KeyUnknown 533 = action (endOfFile False) layout buffers
    | k == Curses.KeyUnknown 538 = action (startOfFile False) layout buffers
    | k == Curses.KeyBTab = action (unTab 4) layout buffers
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
    let sel = selectionCursor $ (\(x, _, _) -> x ) $ active buffers
    let layout' = mapL (changeOffset' crs lnOffset) layout
    let w@(TextView _ _ moo@(rr, cc)) = primaryPane layout'
    -- debug (utilityBar layout') $ show w
    -- debug (utilityBar layout') $ (show $ ebSelection $ active buffers)
    printText' w (ebSelection $ active buffers) (ebToString $ active buffers)
    updateCursor w (rr, cc - lnOffset) crs
    let (_, title, _) = active buffers
    setTitle (titleBar layout') title
    -- (show crs) ++ " " ++ (show sel) ++ " " ++ (show moo)
    refresh w
    c <- Curses.getCh
    debug (utilityBar layout') $ B.pack $ show c
    handleKey c layout' buffers

end :: IO ()
end = Curses.endWin

main :: IO ()
main = do
    args <- getArgs
    let path = (\x -> if (length x) == 0 then "" else head x) args
    start path
    -- or bracket pattern?