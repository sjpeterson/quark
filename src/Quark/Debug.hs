module Quark.Debug where

dbgFile = "/home/stefan/quarkDebug.txt"

dbgClear :: IO ()
dbgClear = writeFile dbgFile ""

dbg :: String -> IO ()
dbg s = appendFile dbgFile s

