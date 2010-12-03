module Main where

import IO
import System

import MicroScheme.Parser

-- Print the usage instructions for this program.
usage = do
  hPutStrLn stderr "Usage: MicroScheme input.ss"
  exitWith (ExitFailure 1)

-- Parse a Scheme file and print the parse tree.
parseFile path = do
  source <- readFile path
  case parseSexps path source of
    Left err     -> do hPutStrLn stderr (show err)
                       exitWith (ExitFailure 2)
    Right output -> putStrLn (show output)

main = do
  argv <- getArgs
  if length argv == 1
    then parseFile (head argv)
    else usage
