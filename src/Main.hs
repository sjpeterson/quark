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
import Control.Monad ( liftM
                     , liftM2 )
                     -- , join )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , makeAbsolute
                        , getCurrentDirectory )
import System.FilePath ( addTrailingPathSeparator
                       , joinPath
                       , splitDirectories )
import System.Clipboard ( setClipboardString
                        , getClipboardString )
import Data.List ( findIndices
                 , findIndex
                 , sort
                 , isPrefixOf )
import Data.Bifunctor ( first )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Quark.Frontend.HSCurses as QFE
import Quark.Window.TitleBar ( setTitle )
import Quark.Window.UtilityBar ( promptString
                               , promptChoice
                               , debug )
import Quark.Window.TextView ( printText
                             , updateOffset )
import Quark.Window.ProjectView ( printTree
                                , updateOffsetP)
import Quark.Layout ( firstL
                    , thirdL
                    , primary )
import Quark.Lexer.Language ( assumeLanguage )
import Quark.Buffer ( Buffer ( Buffer )
                    , ExtendedBuffer
                    , ebContents
                    , ebCursors
                    , ebUnsaved
                    , ebEmpty
                    , ebNew
                    , ebFirst
                    , path
                    , language
                    , tokens
                    , bracketPair
                    , writeProtected
                    , setPath
                    , offset
                    , setOffset
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
                     , projectRoot
                     , projectTree
                     , findDefault
                     , replaceDefault
                     , insertMode
                     , assumeRoot
                     , setRoot
                     , setFindDefault
                     , setReplaceDefault
                     , setProjectTree
                     , setBuffers
                     , toggleInsertMode
                     , activeP
                     , active'
                     , replaceActive'
                     , activeTree
                     , idxOfActive'
                     , activePath
                     , firstF'
                     , firstTree
                     , flipNext'
                     , flipNext''
                     , flipPrevious'
                     , flipTo'
                     , flipTo''
                     , flipToParent'
                     , expand
                     , contract
                     , isSubtree
                     , rootPath )
import Quark.History ( emptyEditHistory )
import Quark.Helpers ( (~~)
                     , lnWidth
                     , nlTail
                     , match )
import Quark.IOHelpers ( listDirectory' )
import Quark.Types (Key ( CharKey
                        , WideCharKey
                        , SpecialKey
                        , CtrlKey
                        , FnKey
                        , ResizeKey )
                   , Direction ( Forward
                               , Backward
                               , Up
                               , Down )
                   , ProjectTree
                   , ProjectTreeElement ( RootElement
                                        , FileElement
                                        , DirectoryElement )
                   , Match ( Exact
                           , Prefix ) )

-- import Quark.Debug ( dbg )

-------------------------------
-- New, open, save and close --
-------------------------------

saveAndQuit :: [Int] -> QFE.Layout -> Project -> IO ()
saveAndQuit [] _ _ = return ()
saveAndQuit (x:xs) layout project' = do
    let w = QFE.primaryPane layout
    let project = first (flipTo x) project'
    let activeBuffer = activeP project
    let cursors = ebCursors $ activeBuffer
    refreshText layout project
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

chooseSave :: QFE.Layout -> Project -> IO (Project, Bool)
chooseSave layout project =
    promptChoice u promptText [ ('y', "Yes", Just True)
                              , ('n', "No", Just False)
                              , ('\ESC', "Cancel", Nothing) ] >>=
        chooseSave' layout project
  where
    u = QFE.utilityBar layout
    promptText = T.pack $ "Save changes to " ++ path' ++ "?"
    path' = path $ activeP project

chooseSave' :: QFE.Layout -> Project -> Maybe Bool -> IO (Project, Bool)
chooseSave' layout project (Just True) = do
    (newProject, canceled) <- if path' == "" then promptSave layout project
                                             else writeP path' project
    if canceled
        then chooseSave layout project
        else if newProject == project
                 then chooseSave' layout project (Just True)
                 else return (newProject, False)
  where
    path' = path $ activeP project

chooseSave' _ project (Just False) = return (project, False)
chooseSave' layout project Nothing =
    QFE.clear u *> QFE.refresh u *> return (project, True)
  where
    u = QFE.utilityBar layout

promptSave :: QFE.Layout -> Project -> IO (Project, Bool)
promptSave layout project = case writeProtected activeBuffer of
    False -> do
        newPath <- liftM T.unpack $
                       promptString u (T.pack "Save buffer to file:") $
                           T.pack path'
        fileExists <- doesFileExist newPath
        let doConfirm = fileExists && path' /= newPath
        dirExists <- doesDirectoryExist newPath
        if dirExists || newPath == ""
            then do debug u $ T.pack $
                        if newPath == ""
                            then "Buffer was not saved"
                            else path' ++ " is a directory"
                    return (project, False)
            else if doConfirm
                     then confirmSave newPath layout project
                     else if newPath == "\ESC"
                              then return (project, True)
                              else do debug u $ T.pack $
                                          "Saved " ++ newPath
                                      writeP newPath project
    True  -> do
        debug u "Can't save protected buffer"
        return (project, False)
  where
    activeBuffer = activeP project
    path' = path $ activeBuffer
    u = QFE.utilityBar layout

confirmSave :: FilePath -> QFE.Layout -> Project -> IO (Project, Bool)
confirmSave newPath layout project = do
    overwrite <- promptChoice u promptText [ ('y', "Yes", True)
                                           , ('n', "No", False) ]
    if overwrite then do debug u $ "Saved " ~~ (T.pack newPath)
                         writeP newPath project
                 else do debug u "Buffer was not saved"
                         return (project, False)
  where
    u = QFE.utilityBar layout
    promptText = (T.pack newPath) ~~ " already exists, overwrite?"

writeP :: FilePath -> Project -> IO (Project, Bool)
writeP path' project = do
    newXB <- writeXB path' $ activeP project
    return (first (replace newXB) project, False)

writeXB :: FilePath -> ExtendedBuffer -> IO ExtendedBuffer
writeXB path' xb@(b, bufferMetaData) = case writeProtected xb of
    False -> do newB <- writeBuffer path' b
                return $ setPath path' (newB, bufferMetaData)
    True  -> return xb

writeBuffer :: FilePath -> Buffer -> IO Buffer
writeBuffer path' (Buffer s h@(n, p, f) crs sel) = do
    TIO.writeFile path' $ nlEnd $ s
    return $ Buffer s (0, p, f) crs sel
  where
    nlEnd s'  = if nlTail s' then s' else s' ~~ "\n"

newBuffer :: QFE.Layout -> Project -> IO ()
newBuffer layout project =
    mainLoop layout $ first (add $ ebEmpty "" "") project

promptOpen :: QFE.Layout -> Project -> IO Project
promptOpen layout project = do
    path' <- liftM T.unpack $
                 promptString u (T.pack "Open file:") $
                 T.pack defaultPath
    if path' == "\ESC"
        then return project
        else openPath path' project
  where
    defaultPath = addTrailingPathSeparator $ projectRoot project
    u = QFE.utilityBar layout

openPath :: FilePath -> Project -> IO Project
openPath path' project@(buffers, _) = do
    -- absPath <- makeAbsolute path'
    case findIndex (\b -> path b == path') $ toList buffers of
        Nothing -> do
            fileExists <- doesFileExist path'
            contents <- if fileExists then TIO.readFile path' else return ""
            return $ first (add $ ebNew path' contents language') project
        Just k  -> return $ first (flipTo k) project
  where
    language' = assumeLanguage path'

----------------------
-- Find and replace --
----------------------

find :: Bool -> Bool -> Bool -> QFE.Layout -> Project -> IO ()
find doReplace next doPrompt layout project = do
    findString <- if doPrompt
                      then promptString u s findDefault'
                      else return findDefault'
    if findString == "\ESC" || findString == ""
        then mainLoop layout project
        else if T.isInfixOf findString (ebContents $ activeP project)
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
    s = T.pack "Find:"
    u = (QFE.utilityBar layout)

replace' :: Bool -> Bool -> QFE.Layout -> Project -> IO ()
replace' next doPrompt layout project = do
    refreshText layout project
    replaceString <- if doPrompt
                         then promptString u s replaceDefault'
                         else return replaceDefault'
    let project' = if replaceString == "\ESC"
                       then project
                       else first (firstF $ ebFirst $ paste replaceString) $
                                setReplaceDefault replaceString project
    find False next False layout project'
  where
    replaceDefault' = replaceDefault project
    s = "Replace by: "
    u = (QFE.utilityBar layout)

-------------------------------------
-- Project tree and root functions --
-------------------------------------

expandIfDir :: QFE.Layout -> Project -> IO ()
expandIfDir layout project = case active' $ projectTree project of
    (RootElement root) -> do
        contents <- listDirectory' root
        projectLoop layout $ firstTree (expand (sort contents)) project
    _                  -> projectLoop layout project

setNewRoot :: Bool -> QFE.Layout -> Project -> IO ()
setNewRoot parent layout project = case active' tree of
    (RootElement root) ->
        if parent then (projectLoop layout) =<< (setRoot' parentDir project)
                  else projectLoop layout $
                           setProjectTree (activeTree tree) $
                           setRoot root project
          where
            parentDir = joinPath $ init $
                        splitDirectories root
    _                  -> projectLoop layout project
  where
    tree = projectTree project

setRoot' :: FilePath -> Project -> IO Project
setRoot' root project = do
    rootContents <- listDirectory' root
    newTree <- expandTree tree (RootElement root, [], sort rootContents)
    return $ setProjectTree newTree $ setRoot root project
  where
    tree = activeTree $ projectTree project

expandTree :: ProjectTree -> ProjectTree -> IO ProjectTree
expandTree originalTree newTree = case isSubtree originalTree newTree of
    True  -> (flipTo'' 0) <$>
                 (replaceActive' $ DirectoryElement originalTree) <$>
                 expandFlipTo (rootPath originalTree) newTree
    False -> pure newTree

expandFlipTo :: FilePath -> ProjectTree -> IO ProjectTree
expandFlipTo path tree = do
    validPath <- liftM2 (||) (doesFileExist path) (doesDirectoryExist path)
    if validPath && isPrefixOf (rootPath tree) path
        then case match path' path of
                 Exact  -> pure tree
                 Prefix -> case active' tree of
                               (RootElement _) -> do c <- listDirectory' path'
                                                     next $ expand c tree
                               _               -> next tree
                 _      -> next tree
        else pure tree
  where
    path' = activePath tree
    next t = expandFlipTo path $ flipNext'' t

-----------------------------
-- Window update functions --
-----------------------------

refreshText :: QFE.Layout -> Project -> IO ()
refreshText layout project = do
    printText (language activeBuffer) w cursors (bracketPair activeBuffer)
        (insertMode project) (tokens activeBuffer)
    QFE.updateCursor w (rr, cc - lnOffset) crs
    QFE.showCursor
    QFE.refresh w
  where
    w@(QFE.TextView _ _ (rr, cc)) = QFE.primaryPane layout
    activeBuffer = activeP project
    cursors@(crs, _) = ebCursors activeBuffer
    lnOffset = lnWidth $ ebContents activeBuffer

refreshTree :: QFE.Layout -> Project -> IO ()
refreshTree (QFE.MinimalLayout _ _ _ _) project = return ()
refreshTree layout project =
   printTree False (QFE.projectPane layout) (projectTree project)

resizeLayout :: (QFE.Layout -> Project -> IO ())
             -> QFE.Layout -> Project -> IO ()
resizeLayout continueFunction layout' project = do
    QFE.onResize
    let lnOffset = lnWidth $ ebContents activeBuffer
    let ((Buffer _ _ crs _), _) = activeBuffer
    layout <- fmap (firstL (updateOffset crs lnOffset)) $ QFE.defaultLayout w
    refreshText layout project
    refreshTree layout project
    continueFunction layout project
  where
    activeBuffer = activeP project
    w = QFE.projectPaneWidth layout'

{-
switchLayout :: QFE.Layout -> Project -> IO ()
switchLayout layout' project = do
    layout <- join $ promptChoice u "Choose layout:" options
    mapM QFE.clear $ QFE.windowList layout'
    mapM QFE.refresh $ QFE.windowList layout'
    refreshText layout project
    refreshTree layout project
    mainLoop layout project
  where
    u = QFE.utilityBar layout'
    options = [ ('1', "Basic", QFE.basicLayout)
              , ('2', "Minimal", QFE.minimalLayout)
              , ('3', "Vertical Split", QFE.vSplitLayout)
              , ('4', "Horizontal Split", QFE.hSplitLayout)
              , ('\ESC', "Cancel", pure layout') ]
-}

toggleLayout :: QFE.Layout -> Project -> IO ()
toggleLayout layout' project = do
    layout <- case layout' of
                  QFE.MinimalLayout _ _ _ _ -> QFE.basicLayout width
                  QFE.BasicLayout _ _ _ _   -> QFE.minimalLayout width
    onNewLayout layout' layout project
  where
    width = QFE.projectPaneWidth layout'

widenProjectPane :: Int -> QFE.Layout -> Project -> IO ()
widenProjectPane k layout' project = do
    layout <- case layout' of
                  QFE.MinimalLayout _ _ _ _ -> QFE.minimalLayout width
                  QFE.BasicLayout _ _ _ _   -> QFE.basicLayout width
    onNewLayout layout' layout project
  where
    width = min maxWidth $ max (QFE.projectPaneWidth layout' + k) 10
    maxWidth = div (QFE.layoutWidth layout') 2
onNewLayout :: QFE.Layout -> QFE.Layout -> Project -> IO ()
onNewLayout oldLayout layout project = do
    mapM QFE.clear $ QFE.windowList oldLayout
    mapM QFE.refresh $ QFE.windowList oldLayout
    refreshText layout project
    refreshTree layout project
    mainLoop layout project

------------------
-- Key handlers --
------------------

handleKey :: QFE.Layout -> Project -> Key -> IO ()
handleKey layout project (CharKey c) =
    mainLoop layout $ first (firstF $ ebFirst $
        input c $ insertMode project) project
handleKey layout project (WideCharKey s) =
    mainLoop layout $ first (firstF $ ebFirst $
        insert (T.pack s) (insertMode project) True) project
handleKey layout project k
    | k == CtrlKey 'q' = saveAndQuit unsavedIndices layout project
    | k == CtrlKey 'w' = saveAndClose layout project
    | k == CtrlKey 's' = do
        (newProject, _) <- promptSave layout project
        mainLoop layout newProject
    | k == CtrlKey 'x' = do
        setClipboardString $ T.unpack $ selection $
            condense $ activeP project
        action delete
    | k == CtrlKey 'c' = do
        setClipboardString $ T.unpack $ selection $
            condense $ activeP project
        mainLoop layout project
    | k == CtrlKey 'v' = do
        s <- getClipboardString
        case s of Nothing -> mainLoop layout project
                  Just s' -> action (paste $ T.pack s')
    | k == CtrlKey 'z' = action undo
    | k == CtrlKey 'y' = action redo
    | k == CtrlKey 'a' = action selectAll
    | k == CtrlKey 'n' = newBuffer layout project
    | k == CtrlKey 'o' = mainLoop layout =<< (promptOpen layout project)
    | k == CtrlKey 'p' = continue -- prompt
    | k == CtrlKey 't' = QFE.hideCursor >> projectLoop layout project
    | k == CtrlKey 'l' = toggleLayout layout project -- switchLayout
    | k == CtrlKey 'j' = widenProjectPane (-1) layout project
    | k == CtrlKey 'k' = widenProjectPane 1 layout project
    | k == CtrlKey 'f' = find False True True layout project
    | k == CtrlKey 'r' = find True True True layout project
    | k == FnKey 3     = find False True False layout project
    | k == FnKey 2     = find False False False layout project
    | k == FnKey 15    = find True True False layout project
    | k == FnKey 14    = find True False False layout project
    | k == SpecialKey "Backspace"       = action backspace
    | k == SpecialKey "Delete"          = action delete
    | k == SpecialKey "Return"          = action nlAutoIndent
    | k == SpecialKey "Tab"             = action' tab
    | k == SpecialKey "Shift-Tab"       = action' unTab
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
    | k == SpecialKey "Insert"          =
          mainLoop layout $ toggleInsertMode project
    | k == ResizeKey                    = resizeLayout mainLoop layout project
    | otherwise = do
          debug (QFE.utilityBar layout) $ T.pack $ show k
          continue
  where
    unsavedIndices = findIndices ebUnsaved $ (\(x, _) -> toList x) project
    (QFE.TextView _ (r, _) _) = QFE.primaryPane layout
    action a = mainLoop layout $ first (firstF (ebFirst a)) project
    action' a = mainLoop layout $ first (firstF a) project
    actionF a = let offset' = QFE.wOffset $ primary layout in do
        let newProject = first (a . firstF (setOffset offset')) project
        let newLayout = firstL (QFE.wSetOffset (offset $ activeP newProject)) layout
        mainLoop newLayout newProject

    -- actionF a = mainLoop layout $ first a project
    continue = mainLoop layout project
    u = QFE.utilityBar layout

handleKeyProject :: QFE.Layout -> Project -> Key -> IO ()
handleKeyProject layout project k
    | k == SpecialKey "Up"        = projectLoop layout $ flipPrevious' 1 project
    | k == SpecialKey "Down"      = projectLoop layout $ flipNext' 1 project
    | k == SpecialKey "Ctrl-Up"   = projectLoop layout $ flipPrevious' 5 project
    | k == SpecialKey "Ctrl-Down" = projectLoop layout $ flipNext' 5 project
    | k == SpecialKey "Esc"       = do
          QFE.showCursor
          dropFocus
          mainLoop layout project
    | k == SpecialKey "Right"     = expandIfDir layout project
    | k == SpecialKey "Left"      = case a of
          (RootElement _) -> projectLoop layout $ contract project
          _               -> projectLoop layout $ flipToParent' project
    | k == SpecialKey "Ctrl-Left" = projectLoop layout $ flipToParent' project
    | k == CharKey '+'            = setNewRoot False layout project
    | k == CharKey '-'            = setNewRoot True layout project
    | k == SpecialKey "Home"      = projectLoop layout $ flipTo' 0 project
    | k == SpecialKey "PgUp"      = projectLoop layout $ flipPrevious' r project
    | k == SpecialKey "PgDn"      = projectLoop layout $ flipNext' r project
    | k == SpecialKey "Return"    = case a of
          (FileElement s)  -> do
              QFE.showCursor
              dropFocus
              (mainLoop layout) =<< (openPath (activePath t) project)
          _                -> expandIfDir layout project
    | k == CtrlKey 'q'            = saveAndQuit unsavedIndices layout project
    | k == ResizeKey              = resizeLayout projectLoop layout project
    | otherwise                   = projectLoop layout project
  where
    t = projectTree project
    a = active' t
    p@(QFE.ProjectView _ (r, _) _) = QFE.projectPane layout
    dropFocus = printTree False p t
    unsavedIndices = findIndices ebUnsaved $ (\(x, _) -> toList x) project

--------------------
-- Loop functions --
--------------------

mainLoop :: QFE.Layout -> Project -> IO ()
mainLoop layout project = do
    let lnOffset = lnWidth $ ebContents activeBuffer
    let ((Buffer _ _ crs _), _) = activeBuffer
    let layout' = firstL (updateOffset crs lnOffset) layout
    setTitle (QFE.titleBar layout') $ if ebUnsaved activeBuffer
                                          then (path activeBuffer) ++ "*"
                                          else path activeBuffer
    refreshText layout' project
    k <- QFE.getKey
    QFE.clear $ QFE.utilityBar layout'
    QFE.refresh $ QFE.utilityBar layout'
    handleKey layout' project k
  where
    activeBuffer = activeP project
    cursors = ebCursors activeBuffer
    frontendFunctions = (QFE.setTextColor, QFE.mvAddString, QFE.refresh)

projectLoop :: QFE.Layout -> Project -> IO ()
projectLoop layout' project = case layout' of
    (QFE.MinimalLayout _ _ _ _) -> QFE.showCursor >> mainLoop layout' project
    _                           -> do
        let layout = thirdL f layout'
        printTree True (QFE.projectPane layout) (projectTree project)
        k <- QFE.getKey
        handleKeyProject layout project k
  where
    f = (updateOffsetP $ idxOfActive' $ projectTree project)

------------------------------
-- Start and main functions --
------------------------------

quarkStart :: (FilePath, FilePath) -> IO ()
quarkStart (root, path') = do
    absPath <- makeAbsolute path'
    absRoot <- makeAbsolute root
    layout <- initLayout
    project' <- initProject absRoot absPath
    tree <- expandFlipTo absPath $ projectTree project'
    let project = setProjectTree tree project'
    refreshTree layout project
    mainLoop layout project

rootAndPath :: [String] -> IO (FilePath, FilePath)
rootAndPath []            = do
    root <- getCurrentDirectory
    return (root, joinPath [root, "Untitled"])
rootAndPath [path']       = do
    isDirectory <- doesDirectoryExist path'
    if isDirectory then return (path', joinPath [path', "Untitled"])
                   else return (assumeRoot path', path')
rootAndPath (root':path':_) = do
    return (root', path')

initLayout :: IO QFE.Layout
initLayout = do
    layout <- QFE.defaultLayout (-1)
    return layout

initBuffer :: FilePath -> IO ExtendedBuffer
initBuffer path' = do
    fileExists <- doesFileExist path'
    if fileExists
        then do contents <- TIO.readFile path'
                return $ ebNew path' contents language'
        else return $ ebEmpty path' language'
  where
    language' = assumeLanguage path'

initProject :: FilePath -> FilePath -> IO Project
initProject root path' = do
    extendedBuffer <- initBuffer path'
    setRoot' root ((extendedBuffer, [], []) , emptyProjectMeta)

main :: IO ()
main = bracket_ QFE.start QFE.end $ getArgs >>= rootAndPath >>= quarkStart