module InscJvm where

import           AbsInstant         (Program)
import           Control.Monad      (unless, when)
import           ErrM
import           Jvm.Compiler       (Pos, compileProgram, runCompiler)
import           LexInstant
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
      outputFile = directory </> fileName <.> "j"
      jasmCmd = "java -jar ./lib/jasmin.jar -d " ++ directory ++ " " ++ outputFile
  unlessM (doesDirectoryExist directory) (failNoDirectory directory)
  unlessM (doesFileExist f) (failNoFile f)
  jasm <- readFile f >>= run v p fileName
  writeFile outputFile jasm
  putStr "Wrote: " >> putStrLn outputFile
  exitCode <- runCommand jasmCmd >>= waitForProcess
  unless (exitCode == ExitSuccess) (failJasm exitCode)
  exitSuccess

failNoDirectory :: FilePath -> IO ()
failNoDirectory d = putStr "Directory not found: " >> putStrLn d >> exitFailure

failNoFile :: FilePath -> IO ()
failNoFile f = putStr "File not found: " >> putStrLn f >> exitFailure

failJasm :: ExitCode -> IO ()
failJasm c = putStr "Jasmin failed with exit code " >> print c >> exitFailure

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
          case runCompiler $ compileProgram name tree of
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
      [ "Instant JVM compiler.",
        "Usage: Call with one of the following argument combinations:",
        "  --help         Display this help message.",
        "  (file)         Compile content of the file into .j and .class files in the file's directory.",
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
