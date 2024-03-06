module Main (main) where

import           Compiler           (DebugMode (..), compile)
import           System.Environment (getArgs)

debug :: DebugMode
debug = Debug

main :: IO ()
main = do
  path <- head <$> getArgs
  compile path debug
