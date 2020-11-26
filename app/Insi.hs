module Insi where

import           AbsInstant
import           Control.Monad      (when)
import           ErrM
import           Interpreter        (interpretProgram, runInterpreter)
import           LexInstant
import           ParInstant
import           PrintInstant
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Show a => Verbosity -> ParseFun (Program a) -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Show a => Verbosity -> ParseFun (Program a) -> String -> IO ()
run v p s =
  let ts = myLLexer s
   in case p ts of
        Bad s -> do
          putStrLn "\nParse              Failed...\n"
          putStrV v "Tokens:"
          putStrV v $ show ts
          putStrLn s
          exitFailure
        Ok tree -> do
          putStrLn "\nParse Successful!"
          showTree v tree
          case runInterpreter $ interpretProgram tree of
            Bad s -> do
              putStrLn "Interpreter failed...\n"
              putStrLn s
              exitFailure
            Ok res -> do
              mapM_ print res
              exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree =
  do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "Instant interpreter.",
        "Usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (no arguments)  Interpret stdin.",
        "  -v              Verbose mode. Interpret stdin verbosely.",
        "  (files)         Interpret content of files.",
        "  -v (files)      Verbose mode. Interpret content of files verbosely."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 0 pProgram
    ["-v"]     -> getContents >>= run 2 pProgram
    "-v" : fs  -> mapM_ (runFile 2 pProgram) fs
    fs         -> mapM_ (runFile 0 pProgram) fs
