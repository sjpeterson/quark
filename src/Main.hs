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

import Control.Exception (bracket_)
import System.Environment ( getArgs )
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , makeAbsolute )
import System.FilePath ( addTrailingPathSeparator )
import System.Clipboard ( setClipboardString
                        , getClipboardString )
import Data.List ( findIndices )
import Data.Bifunctor ( first )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B

import qualified Quark.Frontend.HSCurses as QFE

import Quark.Window.TitleBar ( setTitle )
import Quark.Window.UtilityBar ( promptString
                               , promptChoice
                               , debug )
import Quark.Window.TextView ( printText
                             , updateOffset )
import Quark.Layout ( firstL )
import Quark.Lexer.Language ( assumeLanguage )
import Quark.Buffer ( Buffer ( Buffer )
                    , ExtendedBuffer
                    , ebToString
                    , ebCursors
                    , ebUnsaved
                    , ebEmpty
                    , moveCursor
                    , moveCursorN
                    , selectMoveCursor
                    , selectAll
                    , selection
                    , input
                    , backspace
                    , delete
                    , tab
                    , unTab
                    , nlAutoIndent
                    , undo
                    , redo
                    , paste
                    , condense
                    , startOfLine
                    , endOfLine
                    , startOfFile
                    , endOfFile )
import Quark.Flipper ( add
                     , remove
                     , replace
                     , flipNext
                     , flipPrevious
                     , flipTo
                     , toList
                     , firstF )
import Quark.Project ( Project
                     , assumeRoot
                     , projectRoot
                     , activeP )
import Quark.History ( fromString
                     , toString )
import Quark.Helpers ( (~~)
                     , lnWidth
                     , nlTail )
import Quark.Types (Key ( CharKey
                        , SpecialKey
                        , CtrlKey
                        , ResizeKey )
                   , Direction ( Forward
                               , Backward
                               , Up
                               , Down ) )

initLayout :: IO (QFE.Layout)
initLayout = do
    layout <- QFE.defaultLayout
    return layout

initBuffer :: String -> IO (ExtendedBuffer)
initBuffer path = do
    fileExists <- doesFileExist path
    if fileExists
        then do contents <- B.readFile path
                return ( (Buffer (fromString contents) (0, 0) (0, 0))
                       , (path, assumeLanguage path, False) )
        else return $ ebEmpty path $ assumeLanguage path

initProject :: String -> IO (Project)
initProject path = do
    extendedBuffer <- initBuffer path
    return ((extendedBuffer, [], []), assumeRoot path)

saveAndQuit :: [Int] -> QFE.Layout -> Project -> IO ()
saveAndQuit [] _ _ = return ()
saveAndQuit (x:xs) layout project' = do
    let w = QFE.primaryPane layout
    let project = first (flipTo x) project'
    let activeBuffer@(_, (_, language, _)) = activeP project
    let cursors = ebCursors $ activeBuffer
    printText language w cursors (ebToString activeBuffer)
    (newProject, cancel) <- chooseSave layout project
    if cancel then mainLoop layout newProject
              else saveAndQuit xs layout newProject

saveAndClose :: QFE.Layout -> Project -> IO ()
saveAndClose layout project = do
    ((newBuffers, _), _) <- if ebUnsaved $ activeP project
                                then chooseSave layout project
                                else return (project, False)
    case remove newBuffers of
        Just newerBuffers -> mainLoop layout ( newerBuffers
                                             , projectRoot project)
        Nothing           -> mainLoop layout ( (ebEmpty "" "", [], [])
                                             , projectRoot project )


newBuffer :: QFE.Layout -> Project -> IO ()
newBuffer layout project = mainLoop layout $ first (add $ ebEmpty "" "") project

promptOpen :: QFE.Layout -> Project -> IO (Project)
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
    u = QFE.utilityBar layout

chooseSave :: QFE.Layout -> Project -> IO (Project, Bool)
chooseSave layout project = do
    save <- promptChoice u promptText [ ('y', "Yes", Just True)
                                      , ('n', "No", Just False)
                                      , ('\ESC', "Cancel", Nothing) ]
    chooseSave' save layout project
  where
    u = QFE.utilityBar layout
    promptText = B.pack $ "Save changes to " ++ path ++ "?"
    (_, (path, _, _)) = activeP project

chooseSave' :: Maybe Bool -> QFE.Layout -> Project -> IO (Project, Bool)
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

promptSave :: QFE.Layout -> Project -> IO (Project, Bool)
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
    u = QFE.utilityBar layout

confirmSave :: FilePath -> QFE.Layout -> Project -> IO (Project, Bool)
confirmSave newPath layout project = do
    overwrite <- promptChoice u promptText [ ('y', "Yes", True)
                                           , ('n', "No", False) ]
    if overwrite then do debug u $ "Saved " ~~ (B.pack newPath)
                         writeP newPath project
                 else do debug u "Buffer was not saved"
                         return (project, False)
  where
    u = QFE.utilityBar layout
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

resizeLayout :: QFE.Layout -> Project -> IO ()
resizeLayout layout' project = do
    layout <- QFE.defaultLayout
    mainLoop layout project
  where
    (_, (path, _, _)) = activeP project

handleKey :: QFE.Layout -> Project -> Key -> IO ()
handleKey layout project (CharKey c) =
    mainLoop layout $ first (firstF $ first $ input c) project
handleKey layout project k
    | k == CtrlKey 'q' = saveAndQuit unsavedIndices layout project
    | k == CtrlKey 'w' = saveAndClose layout project
    | k == CtrlKey 's' = do
        (newProject, _) <- promptSave layout project
        mainLoop layout newProject
    | k == CtrlKey 'x' = do
        setClipboardString $ B.unpack $ selection $ condense $ activeP project
        action delete
    | k == CtrlKey 'c' = do
        setClipboardString $ B.unpack $ selection $ condense $ activeP project
        mainLoop layout project
    | k == CtrlKey 'v' = do
        s <- getClipboardString
        case s of Nothing -> mainLoop layout project
                  Just s' -> action (paste $ B.pack s')
    | k == CtrlKey 'z' = action undo
    | k == CtrlKey 'y' = action redo
    | k == CtrlKey 'a' = action selectAll
    | k == CtrlKey 'n' = newBuffer layout project
    | k == CtrlKey 'o' = do
        newProject <- promptOpen layout project
        mainLoop layout newProject
    | k == SpecialKey "Backspace" = action backspace
    | k == SpecialKey "Delete"    = action delete
    | k == SpecialKey "Return"    = action nlAutoIndent
    | k == SpecialKey "Tab"       = action (tab 4)
    | k == SpecialKey "Shift-Tab"      = action (unTab 4)
    | k == SpecialKey "Up"        = action (moveCursor Up)
    | k == SpecialKey "Down"      = action (moveCursor Down)
    | k == SpecialKey "Left"      = action (moveCursor Backward)
    | k == SpecialKey "Right"     = action (moveCursor Forward)
    | k == SpecialKey "Shift-PgUp"     = action (selectMoveCursor Up)
    | k == SpecialKey "Shift-PgDn"     = action (selectMoveCursor Down)
    | k == SpecialKey "Shift-Left"     = action (selectMoveCursor Backward)
    | k == SpecialKey "Shift-Right"    = action (selectMoveCursor Forward)
    | k == SpecialKey "PgUp"      = action (moveCursorN (r - 1) Up)
    | k == SpecialKey "PgDn"      = action (moveCursorN (r - 1) Down)
    | k == SpecialKey "End"       = action (endOfLine True)
    | k == SpecialKey "Home"      = action (startOfLine True)
    | k == SpecialKey "Shift-End"      = action (endOfLine False)
    | k == SpecialKey "Shift-Home"     = action (startOfLine False)
    | k == SpecialKey "Ctrl-End"     = action (endOfFile True)
    | k == SpecialKey "Ctrl-Home"    = action (startOfFile True)
    | k == SpecialKey "Ctrl-Shift-End"    = action (endOfFile False)
    | k == SpecialKey "Ctrl-Shift-Home"   = action (startOfFile False)
    | k == SpecialKey "Ctrl-Left"    = actionF flipPrevious
    | k == SpecialKey "Ctrl-Right"   = actionF flipNext
    | k == ResizeKey         = resizeLayout layout project
    | otherwise = do
                      debug (QFE.utilityBar layout) $ B.pack $ show k
                      continue
  where
    unsavedIndices = findIndices ebUnsaved $ (\(x, _) -> toList x) project
    ((Buffer h _ _), _) = activeP project
    (QFE.TextView _ (r, _) _) = QFE.primaryPane layout
    action a = mainLoop layout $ first (firstF (first a)) project
    actionF a = mainLoop layout $ first a project
    continue = mainLoop layout project
    u = QFE.utilityBar layout

quarkStart :: String -> IO ()
quarkStart path = do
    absPath <- makeAbsolute path
    layout <- initLayout
    project <- initProject absPath
    mainLoop layout project

mainLoop :: QFE.Layout -> Project -> IO ()
mainLoop layout project = do
    let lnOffset = lnWidth $ ebToString activeBuffer
    let ((Buffer h crs sel), (path, language, _)) = activeBuffer
    let layout' = firstL (updateOffset crs lnOffset) layout
    let w@(QFE.TextView _ _ (rr, cc)) = QFE.primaryPane layout'
    setTitle (QFE.titleBar layout') path
    printText language w cursors (ebToString activeBuffer)
    QFE.updateCursor w (rr, cc - lnOffset) crs
    QFE.getKey >>= handleKey layout' project
  where
    activeBuffer = activeP project
    cursors = ebCursors activeBuffer
    frontendFunctions = (QFE.setTextColor, QFE.mvAddString, QFE.refresh)

main :: IO ()
main = bracket_ QFE.start QFE.end $
    (\x -> if (length x) == 0 then "" else head x) <$> getArgs >>= quarkStart