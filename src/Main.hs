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

import Control.Exception ( bracket_ )
import Control.Monad ( liftM )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , makeAbsolute )
import System.FilePath ( addTrailingPathSeparator )
import System.Clipboard ( setClipboardString
                        , getClipboardString )
import Data.List ( findIndices )
import Data.Bifunctor ( first )
import Data.ByteString.UTF8 ( ByteString )
import qualified Data.ByteString.UTF8 as U
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
                    , deselect
                    , selection
                    , input
                    , insert
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
                    , endOfFile
                    , bufferFind )
import Quark.Flipper ( add
                     , remove
                     , replace
                     , flipNext
                     , flipPrevious
                     , flipTo
                     , toList
                     , firstF )
import Quark.Project ( Project
                     , emptyProjectMeta
                     , findDefault
                     , replaceDefault
                     , assumeRoot
                     , setRoot
                     , setFindDefault
                     , setReplaceDefault
                     , projectRoot
                     , setBuffers
                     , activeP )
import Quark.History ( fromString
                     , toString )
import Quark.Helpers ( (~~)
                     , lnWidth
                     , nlTail )
import Quark.Types (Key ( CharKey
                        , WideCharKey
                        , SpecialKey
                        , CtrlKey
                        , FnKey
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
    return $ setRoot (assumeRoot path) ( (extendedBuffer, [], [])
                                       ,  emptyProjectMeta)

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
        Just newerBuffers -> mainLoop layout $ setBuffers newerBuffers project
        Nothing           -> mainLoop layout $ setBuffers ( ebEmpty "" ""
                                                          , []
                                                          , []) project

newBuffer :: QFE.Layout -> Project -> IO ()
newBuffer layout project = mainLoop layout $ first (add $ ebEmpty "" "") project

promptOpen :: QFE.Layout -> Project -> IO (Project)
promptOpen layout project = do
    path <- liftM U.toString $
                promptString u (U.fromString "Open file:") $
                U.fromString defaultPath
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
chooseSave layout project =
    promptChoice u promptText [ ('y', "Yes", Just True)
                              , ('n', "No", Just False)
                              , ('\ESC', "Cancel", Nothing) ] >>=
        chooseSave' layout project
  where
    u = QFE.utilityBar layout
    promptText = U.fromString $ "Save changes to " ++ path ++ "?"
    (_, (path, _, _)) = activeP project

chooseSave' :: QFE.Layout -> Project -> Maybe Bool -> IO (Project, Bool)
chooseSave' layout project (Just True) = do
    (newProject, canceled) <- if path == "" then promptSave layout project
                                            else writeP path project
    if canceled
        then chooseSave layout project
        else if newProject == project
                 then chooseSave' layout project (Just True)
                 else return (newProject, False)
  where
    (_, (path, _, _)) = activeP project
chooseSave' _ project (Just False) = return (project, False)
chooseSave' layout project Nothing =
    QFE.clear u *> QFE.refresh u *> return (project, True)
  where
    u = QFE.utilityBar layout

promptSave :: QFE.Layout -> Project -> IO (Project, Bool)
promptSave layout project = case activeP project of
    (b@(Buffer h _ _), (path, _, False)) -> do
        newPath <- liftM U.toString $
                       promptString u (U.fromString "Save buffer to file:") $
                           U.fromString path
        fileExists <- doesFileExist newPath
        let doConfirm = fileExists && path /= newPath
        dirExists <- doesDirectoryExist newPath
        if dirExists || newPath == ""
            then do debug u $ U.fromString $ if newPath == ""
                                           then "Buffer was not saved"
                                           else path ++ " is a directory"
                    return (project, False)
            else if doConfirm
                     then confirmSave newPath layout project
                     else if newPath == "\ESC"
                              then return (project, True)
                              else do debug u $ U.fromString $
                                          "Saved " ++ newPath
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
    if overwrite then do debug u $ "Saved " ~~ (U.fromString newPath)
                         writeP newPath project
                 else do debug u "Buffer was not saved"
                         return (project, False)
  where
    u = QFE.utilityBar layout
    promptText = (U.fromString newPath) ~~ " already exists, overwrite?"

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

find :: Bool -> Bool -> Bool -> QFE.Layout -> Project -> IO ()
find doReplace next doPrompt layout project = do
    findString <- if doPrompt
                      then promptString u s findDefault'
                      else return findDefault'
    if findString == "\ESC" || findString == ""
        then mainLoop layout project
        else if B.isInfixOf findString (ebToString $ activeP project)
                 then nextFunction $
                          first (firstF $ first $
                              bufferFind next (not doReplace) findString) $
                                  setFindDefault findString project
                 else do debug u $ "No match for \"" ~~ findString ~~ "\""
                         mainLoop layout $ setFindDefault findString project
  where
    nextFunction = if doReplace
                       then replace' next doPrompt layout
                       else mainLoop layout
    findDefault' = findDefault project
    s = B.pack "Find:"
    u = (QFE.utilityBar layout)

replace' :: Bool -> Bool -> QFE.Layout -> Project -> IO ()
replace' next doPrompt layout project = do
    refreshText layout project
    replaceString <- if doPrompt
                         then promptString u s replaceDefault'
                         else return replaceDefault'
    let project' = if replaceString == "\ESC"
                       then project
                       else first (firstF $ first $ paste replaceString) $
                                setReplaceDefault replaceString project
    -- mainLoop layout project'
    find False next False layout project'
  where
    replaceDefault' = replaceDefault project
    s = B.pack "Replace by: "
    u = (QFE.utilityBar layout)

resizeLayout :: QFE.Layout -> Project -> IO ()
resizeLayout layout' project = do
    layout <- QFE.defaultLayout
    mainLoop layout project
  where
    (_, (path, _, _)) = activeP project

handleKey :: QFE.Layout -> Project -> Key -> IO ()
handleKey layout project (CharKey c) =
    mainLoop layout $ first (firstF $ first $ input c) project
handleKey layout project (WideCharKey s) =
    mainLoop layout $ first (firstF $ first $ insert (B.pack s) True) project
handleKey layout project k
    | k == CtrlKey 'q' = saveAndQuit unsavedIndices layout project
    | k == CtrlKey 'w' = saveAndClose layout project
    | k == CtrlKey 's' = do
        (newProject, _) <- promptSave layout project
        mainLoop layout newProject
    | k == CtrlKey 'x' = do
        setClipboardString $ U.toString $ selection $ condense $ activeP project
        action delete
    | k == CtrlKey 'c' = do
        setClipboardString $ U.toString $ selection $ condense $ activeP project
        mainLoop layout project
    | k == CtrlKey 'v' = do
        s <- getClipboardString
        case s of Nothing -> mainLoop layout project
                  Just s' -> action (paste $ U.fromString s')
    | k == CtrlKey 'z' = action undo
    | k == CtrlKey 'y' = action redo
    | k == CtrlKey 'a' = action selectAll
    | k == CtrlKey 'n' = newBuffer layout project
    | k == CtrlKey 'o' = mainLoop layout =<< (promptOpen layout project)
    | k == CtrlKey 'p' = continue -- prompt
    | k == CtrlKey 'f' = find False True True layout project
    | k == CtrlKey 'r' = find True True True layout project
    | k == FnKey 3     = find False True False layout project
    | k == FnKey 2     = find False False False layout project
    | k == FnKey 15    = find True True False layout project
    | k == FnKey 14    = find True False False layout project
    | k == SpecialKey "Backspace"       = action backspace
    | k == SpecialKey "Delete"          = action delete
    | k == SpecialKey "Return"          = action nlAutoIndent
    | k == SpecialKey "Tab"             = action (tab 4)
    | k == SpecialKey "Shift-Tab"       = action (unTab 4)
    | k == SpecialKey "Up"              = action (moveCursor Up)
    | k == SpecialKey "Down"            = action (moveCursor Down)
    | k == SpecialKey "Left"            = action (moveCursor Backward)
    | k == SpecialKey "Right"           = action (moveCursor Forward)
    | k == SpecialKey "Shift-PgUp"      = action (selectMoveCursor Up)
    | k == SpecialKey "Shift-PgDn"      = action (selectMoveCursor Down)
    | k == SpecialKey "Shift-Left"      = action (selectMoveCursor Backward)
    | k == SpecialKey "Shift-Right"     = action (selectMoveCursor Forward)
    | k == SpecialKey "PgUp"            = action (moveCursorN (r - 1) Up)
    | k == SpecialKey "PgDn"            = action (moveCursorN (r - 1) Down)
    | k == SpecialKey "End"             = action (endOfLine True)
    | k == SpecialKey "Home"            = action (startOfLine True)
    | k == SpecialKey "Shift-End"       = action (endOfLine False)
    | k == SpecialKey "Shift-Home"      = action (startOfLine False)
    | k == SpecialKey "Ctrl-End"        = action (endOfFile True)
    | k == SpecialKey "Ctrl-Home"       = action (startOfFile True)
    | k == SpecialKey "Ctrl-Shift-End"  = action (endOfFile False)
    | k == SpecialKey "Ctrl-Shift-Home" = action (startOfFile False)
    | k == SpecialKey "Ctrl-Left"       = actionF flipPrevious
    | k == SpecialKey "Ctrl-Right"      = actionF flipNext
    | k == SpecialKey "Esc"             = action deselect
    | k == ResizeKey                    = resizeLayout layout project
    | otherwise = do
          debug (QFE.utilityBar layout) $ U.fromString $ show k
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
    let ((Buffer _ crs _), (path, _, _)) = activeBuffer
    let layout' = firstL (updateOffset crs lnOffset) layout
    setTitle (QFE.titleBar layout') $ if ebUnsaved activeBuffer
                                          then path ++ "*"
                                          else path
    refreshText layout' project
    k <- QFE.getKey
    QFE.clear $ QFE.utilityBar layout'
    QFE.refresh $ QFE.utilityBar layout'
    handleKey layout' project k
  where
    activeBuffer = activeP project
    cursors = ebCursors activeBuffer
    frontendFunctions = (QFE.setTextColor, QFE.mvAddString, QFE.refresh)

refreshText :: QFE.Layout -> Project -> IO ()
refreshText layout project = do
    printText language w cursors (ebToString activeBuffer)
    QFE.updateCursor w (rr, cc - lnOffset) crs
  where
    w@(QFE.TextView _ _ (rr, cc)) = QFE.primaryPane layout
    activeBuffer@((Buffer _ crs _), (_, language, _)) = activeP project
    cursors = ebCursors activeBuffer
    lnOffset = lnWidth $ ebToString activeBuffer

main :: IO ()
main = bracket_ QFE.start QFE.end $
    (\x -> if (length x) == 0 then "" else head x) <$> getArgs >>= quarkStart
