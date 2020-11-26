module InscLlvm where

import           AbsInstant         (Program)
import           Control.Monad      (unless, when)
import           ErrM
import           LexInstant
import           Llvm.Compiler      (Pos, compileProgram, runCompiler)
import           ParInstant
import           PrintInstant
import           System.Directory   (doesDirectoryExist, doesFileExist)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitFailure, exitSuccess)
import           System.FilePath    (dropExtension, takeDirectory, takeFileName,
                                     (<.>), (</>))
import           System.Process     (runCommand, waitForProcess)

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p a = do
  b <- p
  unless b a

runFile :: Verbosity -> ParseFun (Program Pos) -> FilePath -> IO ()
runFile v p f = do
  let fileName = dropExtension $ takeFileName f
      directory = takeDirectory f
      outputLlFile = directory </> fileName <.> "ll"
      outputBcFile = directory </> fileName <.> "bc"
      llvmCmd = "llvm-as -o " ++ outputBcFile ++ " " ++ outputLlFile
  unlessM (doesDirectoryExist directory) (failNoDirectory directory)
  unlessM (doesFileExist f) (failNoFile f)
  ll <- readFile f >>= run v p fileName
  writeFile outputLlFile ll
  putStr "Wrote: " >> putStrLn outputLlFile
  exitCode <- runCommand llvmCmd >>= waitForProcess
  unless (exitCode == ExitSuccess) (failLlvm exitCode)
  putStr "Wrote: " >> putStrLn outputBcFile
  exitSuccess

failNoDirectory :: FilePath -> IO ()
failNoDirectory d = putStr "Directory not found: " >> putStrLn d >> exitFailure

failNoFile :: FilePath -> IO ()
failNoFile f = putStr "File not found: " >> putStrLn f >> exitFailure

failLlvm :: ExitCode -> IO ()
failLlvm c = putStr "LLVM assembler failed with exit code " >> print c >> exitFailure

run :: Verbosity -> ParseFun (Program Pos) -> String -> String -> IO String
run v p name s =
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
          case runCompiler $ compileProgram tree of
            Bad s -> do
              putStrLn "Compilation failed...\n"
              putStrLn s
              exitFailure
            Ok res -> return $ unlines res

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree =
  do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "Instant LLVM compiler.",
        "Usage: Call with one of the following argument combinations:",
        "  --help         Display this help message.",
        "  (file)         Compile content of the file into .ll and .bc files in the file's directory.",
        "  -v (file)      Verbose mode. Compile content of the file verbosely."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-v" : [f] -> runFile 2 pProgram f
    [f]        -> runFile 0 pProgram f
    _          -> usage
