This simple program demonstrates the quark user interface.

make sure environment variable TERM is xterm-256color

We begin with some imports.

\begin{code}
module Interface where
-- import Control.Exception (bracket_)
import qualified UI.HSCurses.Curses as Curses
-- import qualified UI.HSCurses.CursesHelper as CursesH
import Windows

\end{code}

Define a neat shorthand for initializing foreground color f and background
color b as color pair n.

\begin{code}
defineColor :: Int -> Int -> Int -> IO ()
defineColor n f b =
  Curses.initPair (Curses.Pair n) (Curses.Color f) (Curses.Color b)
\end{code}

Initialize default color pairs from palette.

1 through 16 are the following foreground colors on default background:

1  - Red
2  - Green
3  - Brown/Dark yellow
4  - Blue
5  - Magenta
6  - Cyan
7  - Light gray
8  - Dark gray
9  - Light red
10 - Light green
11 - Yellow
12 - Light blue
13 - Light magenta
14 - Light cyan
15 - White
16 - Black

17 is for the title bar.

\begin{code}
defineColors = do
  mapM_ (\n -> defineColor n n (-1)) [1..16]
  -- defineColor 17 16 7
  defineColor 17 16 2
  return ()
\end{code}

We define four windows. The title bar spans the top row, the utility bar spans
the two bottom rows. The project- and main view span the remaining space
vertically and splits the horizontal space. First, the main view is given at
least 85 characters. The project view is given up to 32 characters and is
hidden if fewer than 16 characters are available for it.

\begin{code}

defaultSplit :: Int -> Int
defaultSplit x
  | x < (85+16) = 0
  | otherwise   = min 24 (x - 32)

initWindows :: IO (Window, Window, Window, Window)
initWindows = do
  (mvRows, mvCols) <- Curses.scrSize
  let pvWidth = defaultSplit mvCols
  let mainHeight = mvRows - 3
  let mainWidth = mvCols - pvWidth
  cTitleBar <- Curses.newWin 2 mvCols 0 0
  cUtilityBar <- Curses.newWin 2 mvCols (mvRows - 3) 0
  cProjectView <- Curses.newWin (mainHeight + 1) pvWidth 0 1
  cMainView <- Curses.newWin (mainHeight + 1) mainWidth 1 pvWidth
  let titleBar = TitleBar cTitleBar (1, mvCols)
  let utilityBar = UtilityBar cUtilityBar (1, mvCols)
  let projectView = ProjectView cProjectView (mainHeight, pvWidth) 0
  let mainView = TextView cMainView (mainHeight, (mvCols - pvWidth)) (0, 0)
  fillBackground titleBar 17
  updateTitleBar cTitleBar mvCols "home"
  printSomeText cMainView
  Curses.wRefresh cMainView
  return (titleBar, utilityBar, projectView, mainView)

printSomeText cMainView = do
  let sampleText = unlines $ [ "defaultSplit :: Int -> Int"
                             , "defaultSplit x"
                             , "    | x < (85+16) == 0"
                             , "    | otherwise   = min 24 (x - 32)" ]
  Curses.wMove cMainView 0 0
  Curses.wAttrSet cMainView (Curses.attr0, Curses.Pair 0)
  Curses.wAddStr cMainView sampleText
  Curses.wRefresh cMainView

fillBackground :: Window -> Int -> IO ()
fillBackground (TitleBar cTitleBar (h, w)) colorId = do
    Curses.wAttrSet cTitleBar (Curses.attr0, Curses.Pair colorId)
    Curses.mvWAddStr cTitleBar 0 0 (take w $ repeat ' ')
    Curses.wMove cTitleBar 1 0
    Curses.wRefresh cTitleBar
    return ()

padToLen :: Int -> [Char] -> [Char]
padToLen k a
  | k <= length a = a
  | otherwise     = padToLen k $ a ++ " "

\end{code}

Some helpful functions for printing

\begin{code}

-- updateTitleBar :: IO Curses.Window -> Int -> [Char] -> Int -> Int -> IO ()
updateTitleBar win maxCols path = do
    Curses.attrSet Curses.attr0 (Curses.Pair 17)
    Curses.mvWAddStr win 0 0 (padMidToLen maxCols leftText rightText)
    Curses.wRefresh win
    return ()
  where
    leftText = " quark - " ++ path
    rightText = "0.0.1a "

padMidToLen :: Int -> [Char] -> [Char] -> [Char]
padMidToLen k a0 a1
  | k == (length a0 + length a1) = a0 ++ a1
  | otherwise                    = padMidToLen k (a0 ++ " ") a1
\end{code}

This is the actual init monad. Here we start curses and initialize our colors.

\begin{code}
start = do
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
  (titleBar, utilityBar, projectView, mainView) <- initWindows
  mainLoop titleBar utilityBar projectView mainView

\end{code}


\begin{code}

mainLoop titleBar utilityBar projectView mainView = do
  c <- Curses.getch
  if Curses.decodeKey c == Curses.KeyChar 'q'
    then end
    else mainLoop titleBar utilityBar projectView mainView

end = do
  Curses.endWin
  return ()

main = do
  start
  return ()
  -- or bracket pattern?

\end{code}
