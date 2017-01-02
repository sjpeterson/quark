\begin{code}
import qualified UI.HSCurses.Curses as Curses

import Windows
\end{code}

\begin{code}
start = do
    Curses.initScr
    mainLoopf

mainLoop = do
    c <- Curses.getch
    if Curses.decodeKey c == Curses.KeyChar 'q'
        then end
        else mainLoop

end = do
    Curses.endWin
    return ()

main = do
    start
    return ()
\end{code}
