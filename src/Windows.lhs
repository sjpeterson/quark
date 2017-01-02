Text

\begin{code}
module Windows where

import qualified UI.HSCurses.Curses as Curses

\end{code}

\begin{code}

type Size = (Int, Int)
type Offset = (Int, Int)

data Window = TitleBar Curses.Window Size
            | UtilityBar Curses.Window Size
            | TextView Curses.Window Size Offset
            | ProjectView Curses.Window Size Int
\end{code}
