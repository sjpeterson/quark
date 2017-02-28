{-# LANGUAGE OverloadedStrings #-}

--------
--
-- File:        Main.hs
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- Quark main file
--
--------

module Main where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , makeAbsolute )
import System.FilePath ( addTrailingPathSeparator )
import System.Clipboard
-- import Control.Exception (bracket_)
import Data.Char ( isPrint )
import Data.List ( findIndices )
import Data.Bifunctor ( first )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import qualified UI.HSCurses.Curses as Curses
-- import qualified UI.HSCurses.CursesHelper as CursesH

import Quark.Window.Core
import Quark.Window.TitleBar
import Quark.Window.UtilityBar
import Quark.Window.TextView
import Quark.Lexer.Language ( assumeLanguage )
import Quark.Layout
import Quark.Buffer
import Quark.Flipper
import Quark.Project
import Quark.History
import Quark.Types
import Quark.Helpers
import Quark.Colors

initLayout :: IO (Layout)
initLayout = do
    layout <- defaultLayout
    fillBackground (titleBar layout) titleBarColor
    return layout

initBuffer :: String -> IO (ExtendedBuffer)
initBuffer path = do
    fileExists <- doesFileExist path
    if fileExists
        then do contents <- B.readFile path
                return ( (Buffer (fromString contents) (0, 0) (0, 0))
                       , (path, assumeLanguage path, False) )
        else return emptyXB

initProject :: String -> IO (Project)
initProject path = do
    extendedBuffer <- initBuffer path
    return ((extendedBuffer, [], []), assumeRoot path)

saveAndQuit :: [Int] -> Layout -> Project -> IO ()
saveAndQuit [] _ _ = end
saveAndQuit (x:xs) layout project' = do
    let w = primaryPane layout
    let project = first (flipTo x) project'
    let activeBuffer@(_, (_, language, _)) = activeP project
    let cursors = ebCursors $ activeBuffer
    printText language w cursors (ebToString activeBuffer)
    (newProject, cancel) <- chooseSave layout project
    if cancel then mainLoop layout newProject
              else saveAndQuit xs layout newProject

saveAndClose :: Layout -> Project -> IO ()
saveAndClose layout project = do
    ((newBuffers, _), _) <- if unsavedXB $ activeP project
                                then chooseSave layout project
                                else return (project, False)
    case remove newBuffers of
        Just newerBuffers -> mainLoop layout ( newerBuffers
                                             , projectRoot project)
        Nothing           -> mainLoop layout ( (emptyXB, [], [])
                                             , projectRoot project )


newBuffer :: Layout -> Project -> IO ()
newBuffer layout project = mainLoop layout $ first (add emptyXB) project

promptOpen :: Layout -> Project -> IO (Project)
promptOpen layout project = do
    path <- promptString u (B.pack "Open file:") $ B.pack defaultPath
    if path == "\ESC"
        then return project
        else do
            fileExists <- doesFileExist path
            contents <- if fileExists then B.readFile path else return ""
            let newBuffer' = ( (Buffer (fromString contents) (0, 0) (0, 0))
                             , (path, assumeLanguage path, False) )
            return $ first (add newBuffer') project
  where
    defaultPath = addTrailingPathSeparator $ projectRoot project
    u = utilityBar layout

chooseSave :: Layout -> Project -> IO (Project, Bool)
chooseSave layout project = do
    save <- promptChoice u promptText [ ('y', "Yes", Just True)
                                      , ('n', "No", Just False)
                                      , ('\ESC', "Cancel", Nothing) ]
    chooseSave' save layout project
  where
    u = utilityBar layout
    promptText = B.pack $ "Save changes to " ++ path ++ "?"
    (_, (path, _, _)) = activeP project

chooseSave' :: Maybe Bool -> Layout -> Project -> IO (Project, Bool)
chooseSave' (Just True) layout project = do
    (newProject, canceled) <- if path == "" then promptSave layout project
                                            else writeP path project
    if canceled
        then chooseSave layout project
        else if newProject == project
                 then chooseSave' (Just True) layout project
                 else return (newProject, False)
  where
    (_, (path, _, _)) = activeP project
chooseSave' (Just False) _ project = return (project, False)
chooseSave' Nothing _ project = return (project, True)

promptSave :: Layout -> Project -> IO (Project, Bool)
promptSave layout project = case activeP project of
    (b@(Buffer h _ _), (path, _, False)) -> do
        newPath <- promptString u (B.pack "Save buffer to file:") $ B.pack path
        fileExists <- doesFileExist newPath
        let doConfirm = fileExists && path /= newPath
        dirExists <- doesDirectoryExist newPath
        if dirExists || newPath == ""
            then do debug u $ B.pack $ if newPath == ""
                                           then "Buffer was not saved"
                                           else path ++ " is a directory"
                    return (project, False)
            else if doConfirm
                     then confirmSave newPath layout project
                     else if newPath == "\ESC"
                              then return (project, True)
                              else do debug u $ B.pack $ "Saved " ++ newPath
                                      writeP newPath project
    _                            -> do
        debug u "Can't save protected buffer"
        return (project, True)
  where
    u = utilityBar layout

confirmSave :: FilePath -> Layout -> Project -> IO (Project, Bool)
confirmSave newPath layout project = do
    overwrite <- promptChoice u promptText [ ('y', "Yes", True)
                                           , ('n', "No", False) ]
    if overwrite then do debug u $ "Saved " ~~ (B.pack newPath)
                         writeP newPath project
                 else do debug u "Buffer was not saved"
                         return (project, False)
  where
    u = utilityBar layout
    promptText = (B.pack newPath) ~~ " already exists, overwrite?"

writeP :: FilePath -> Project -> IO (Project, Bool)
writeP path project = do
    newXB <- writeXB path $ activeP project
    return (first (replace newXB) project, False)

writeXB :: FilePath -> ExtendedBuffer -> IO ExtendedBuffer
writeXB path (b, (_, language, False)) = do
    newB <- writeBuffer path b
    return $ (newB, (path, language, False))
writeXB _ xb@(_, (_, _, True)) = return xb

writeBuffer :: FilePath -> Buffer -> IO Buffer
writeBuffer path (Buffer h@(n, p, f) c s) = do
    B.writeFile path $ nlEnd $ toString h
    return $ Buffer (0, p, f) c s
  where
    nlEnd s  = if nlTail s then s else s ~~ "\n"

resizeLayout :: Layout -> Project -> IO ()
resizeLayout layout' project = do
    --clearLayout layout'
    --Curses.refresh
    layout <- defaultLayout
    fillBackground (titleBar layout) titleBarColor
    mainLoop layout project
  where
    (_, (path, _, _)) = activeP project

handleKey :: Curses.Key -> Layout -> Project -> IO ()
handleKey k layout project
    | k == translateKey "C-q"       = saveAndQuit unsavedIndices layout project
    | k == translateKey "C-w"       = saveAndClose layout project
    | k == translateKey "C-s"       = do
        (newProject, _) <- promptSave layout project
        mainLoop layout newProject
    | k == translateKey "C-x"       = do
        setClipboardString $ B.unpack $ selection $ condense $ activeP project
        action delete
    | k == translateKey "C-c"       = do
        setClipboardString $ B.unpack $ selection $ condense $ activeP project
        mainLoop layout project
    | k == translateKey "C-v"       = do
        s <- getClipboardString
        case s of Nothing -> mainLoop layout project
                  Just s' -> action (paste $ B.pack s')
    | k == translateKey "C-z"       = action undo
    | k == translateKey "C-y"       = action redo
    | k == translateKey "C-a"       = action selectAll
    | k == translateKey "C-n"       = newBuffer layout project
    | k == translateKey "C-o"       = do
        newProject <- promptOpen layout project
        mainLoop layout newProject
    | k == translateKey "Backspace" = action backspace
    | k == translateKey "Delete"    = action delete
    | k == translateKey "Return"    = action nlAutoIndent
    | k == translateKey "Tab"       = action (tab 4)
    | k == translateKey "^Tab"      = action (unTab 4)
    | k == translateKey "Up"        = action (moveCursor Up)
    | k == translateKey "Down"      = action (moveCursor Down)
    | k == translateKey "Left"      = action (moveCursor Backward)
    | k == translateKey "Right"     = action (moveCursor Forward)
    | k == translateKey "^PgUp"     = action (selectMoveCursor Up)
    | k == translateKey "^PgDn"     = action (selectMoveCursor Down)
    | k == translateKey "^Left"     = action (selectMoveCursor Backward)
    | k == translateKey "^Right"    = action (selectMoveCursor Forward)
    | k == translateKey "PgUp"      = action (moveCursorN (r - 1) Up)
    | k == translateKey "PgDn"      = action (moveCursorN (r - 1) Down)
    | k == translateKey "End"       = action (endOfLine True)
    | k == translateKey "Home"      = action (startOfLine True)
    | k == translateKey "^End"      = action (endOfLine False)
    | k == translateKey "^Home"     = action (startOfLine False)
    | k == translateKey "C-End"     = action (endOfFile True)
    | k == translateKey "C-Home"    = action (startOfFile True)
    | k == translateKey "C-^End"    = action (endOfFile False)
    | k == translateKey "C-^Home"   = action (startOfFile False)
    | k == translateKey "C-Left"    = actionF flipPrevious
    | k == translateKey "C-Right"   = actionF flipNext
    | k == Curses.KeyResize         = resizeLayout layout project
    | otherwise                     = case k of
          (Curses.KeyChar c) -> case isPrint c of
                                    True -> action (input c)
                                    False -> do debug u $ B.pack $ show k
                                                continue
          _ -> do debug (utilityBar layout) $ B.pack $ show k
                  continue
  where
    unsavedIndices = findIndices unsavedXB $ (\(x, _) -> toList x) project
    ((Buffer h _ _), _) = activeP project
    (TextView _ (r, _) _) = primaryPane layout
    action a = mainLoop layout $ first (firstF (first a)) project
    actionF a = mainLoop layout $ first a project
    continue = mainLoop layout project
    u = utilityBar layout

start :: String -> IO ()
start path = do
    Curses.initScr
    hasColors <- Curses.hasColors
    if hasColors then do Curses.startColor
                         Curses.useDefaultColors
                         defineColors
                 else return ()
    Curses.resetParams
    Curses.wclear Curses.stdScr
    Curses.refresh
    absPath <- makeAbsolute path
    layout <- initLayout
    project <- initProject absPath
    mainLoop layout project

mainLoop :: Layout -> Project -> IO ()
mainLoop layout project = do
    let lnOffset = lnWidth $ ebToString activeBuffer
    let ((Buffer h crs sel), (path, language, _)) = activeBuffer
    let layout' = firstL (updateOffset crs lnOffset) layout
    let w@(TextView _ _ (rr, cc)) = primaryPane layout'
    setTitle (titleBar layout') $ path ++ (show crs)
    printText language w cursors (ebToString activeBuffer)
    updateCursor w (rr, cc - lnOffset) crs
    c <- Curses.getCh
    handleKey c layout' project
  where
    activeBuffer = activeP project
    cursors = ebCursors activeBuffer

end :: IO ()
end = Curses.endWin

main :: IO ()
main = (\x -> if (length x) == 0 then "" else head x) <$> getArgs >>= start