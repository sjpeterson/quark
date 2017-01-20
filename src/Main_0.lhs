The main file

\begin{code}
-- import Control.Exception (bracket_)
-- import qualified UI.HSCurses.Curses as Curses
-- import qualified UI.HSCurses.CursesHelper as CursesH

\end{code}

This definition of Layout is just temporary for testing purposes

\begin{code}

data Layout = Layout Int Char
data Action = SetChar | SetInt

getLayoutContents :: Layout -> (Char, Int)
getLayoutContents (Layout x c) = (c, x)

perform :: Action -> Char -> Layout -> Layout
perform a c layout = case a of
     SetChar -> layoutSetChar c layout
     SetInt  -> layoutSetInt c layout

layoutSetChar :: Char -> Layout -> Layout
layoutSetChar c (Layout x _) = Layout x c

layoutSetInt :: Char -> Layout -> Layout
layoutSetInt _ (Layout x c0) = Layout x c0

printLayout :: Layout -> IO ()
printLayout (Layout x c) = putStrLn $ "Current state: " ++ (show x) ++ ", " ++ (show c)

\end{code}

Now here starts the actual code

\begin{code}
start :: IO (Layout)
start = do
    return (Layout 4 'c')

end :: IO ()
end = do
    return ()

mainLoop :: Layout -> IO ()
mainLoop layout = do
    c <- getChar    -- replace with curses getch
    let newLayout = perform SetChar c layout
    printLayout newLayout
    -- action <- getAction c
    -- newLayout <- perform action c layout
    mainLoop newLayout

main :: IO ()
main = do
    layout <- start
    mainLoop layout
    end
    return ()

\end{code}