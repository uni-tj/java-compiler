{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
module Error.PrintError (printError, File, FileContainer(..), throwPretty) where

import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.State  (MonadState, gets)
import           Data.Function.Syntax (slipr)
import           Data.Functor         ((<&>))
import           Data.List.Extra      (intercalate)
import           System.Console.ANSI  (Color (Red), ColorIntensity (Vivid),
                                       ConsoleLayer (Foreground),
                                       SGR (Reset, SetColor), setSGRCode)
import           Types.AST            (Position (..))

{- General helpers -}
{- Slice a list
    Start is inclusive
    End is exclusive
-}
slice :: Int -> Int -> [a] -> [a]
slice s e xs
  = take (e - s) $ drop s xs
{- General helpers end -}

line :: (a, b) -> a
line = fst
col :: (a, b) -> b
col = snd

type File = [String]
type Line = (Int, String)

zipLine :: File -> [Line]
zipLine = zip [1..]

printMeta :: Int -> String -> String
printMeta ln content = pad 3 (show ln) ++ " | " ++ content
  where pad target str = replicate (target - length str) ' ' ++ str

printRed :: Position -> [Line] -> [Line]
printRed pos = fmap $ \(ln, str) ->
  if line (start pos) > ln || line (end pos) < ln
    then (ln, str)
    else
      let startCol = if ln == line (start pos) then col $ start pos else 1
          endCol   = if ln == line (end   pos) then col $ end   pos else length str + 1
      in (ln, take (startCol - 1) str ++ red (slice (startCol - 1) (endCol-1) str) ++ drop (endCol-1) str )

red :: String -> String
red str = setSGRCode [SetColor Foreground Vivid Red] ++ str ++ setSGRCode [Reset]

printError :: File -> String -> Position -> String
printError fl msg pos
  = (red ("Error: " ++ msg ++ "\n\n") ++)
  $ (++ "\n")
  $ intercalate "\n"
  $ slice (startLn - 1) lastLn
  $ fmap (uncurry printMeta)
  $ printRed pos
  $ zipLine fl
  where startLn = max 1 $ subtract 3 $ line (start pos)
        lastLn   = min (length fl) $ (+3) $ line (end pos)

class FileContainer c where
  file :: c -> File

throwPretty :: (FileContainer c, MonadError String m, MonadState c m) => String -> Position -> m a
throwPretty msg pos = gets file <&> slipr printError msg pos >>= throwError

test = putStrLn $ printError
  [ "123456789"
  , "123"
  , "123"
  , "123"
  , "12345"
  , ""
  , "123"
  , "123"
  , "123"
  , "123"
  , "123"
  ]
  "You did something wrong."
  Position {start=(5,2), end=(7,2)}
