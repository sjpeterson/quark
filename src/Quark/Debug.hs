module Quark.Debug where

import System.Directory ( getHomeDirectory )
import System.FilePath ( joinPath )

dbgFile :: IO FilePath
dbgFile = (\h -> joinPath [h, "quarkDebug.log"]) <$> getHomeDirectory

dbgClear :: IO ()
dbgClear = (\p -> writeFile p "") =<< dbgFile

dbg :: String -> IO ()
dbg s = (\p -> appendFile p s) =<< dbgFile

