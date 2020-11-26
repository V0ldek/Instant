module Jvm.Compiler
  ( Pos,
    runCompiler,
    compileProgram,
  )
where

import           AbsInstant
import           Control.Monad.State
import           Data.List           (intercalate)
import           Data.Map            as Map
import           ErrM
import           Jvm.Abs
import           Jvm.Emit            as Emit

type Loc = Int

type Store = Map.Map Ident Loc

type Compiler = StateT Store Err

runCompiler :: Compiler a -> Err a
runCompiler i = evalStateT i Map.empty

compileProgram :: String -> Program Pos -> Compiler [String]
compileProgram name prog = run $ annotate prog
  where
    run (Prog meta stmts) = do
      stmtCode <- mapM compileStmt stmts
      localCount <- getLocCount
      let progStackLimit = stackLimit meta
          mainBody = concat stmtCode
          codeBlocks =
            [ Emit.classHeader name,
              Emit.defaultCtor,
              Emit.mainHeader localCount progStackLimit,
              mainBody,
              Emit.endMain
            ]
          code = intercalate [""] codeBlocks
      return code

compileStmt :: Show a => Stmt a -> Compiler [String]
compileStmt (SExp _ exp) = do
  expCode <- compileExp exp
  return $ expCode ++ Emit.printInt
compileStmt (SAss _ ident exp) = do
  expCode <- compileExp exp
  loc <- getOrCreateLoc ident
  let assCode = Emit.store loc
  return $ expCode ++ assCode

compileExp :: Show a => Exp a -> Compiler [String]
compileExp (ExpLit _ n) = return $ Emit.loadInt (fromInteger n)
compileExp (ExpVar a ident) = do
  mbloc <- gets (Map.lookup ident)
  case mbloc of
    Just loc -> return $ Emit.load loc
    Nothing -> fail $ "Unassigned variable '" ++ show ident ++ "' at " ++ show a
compileExp (ExpAdd _ exp1 exp2) = compileBinExp Emit.add exp1 exp2
compileExp (ExpSub _ exp1 exp2) = compileBinExp Emit.sub exp1 exp2
compileExp (ExpMul _ exp1 exp2) = compileBinExp Emit.mul exp1 exp2
compileExp (ExpDiv _ exp1 exp2) = compileBinExp Emit.div exp1 exp2

compileBinExp :: Show a => [String] -> Exp a -> Exp a -> Compiler [String]
compileBinExp binCode exp1 exp2 = do
  exp1Code <- compileExp exp1
  exp2Code <- compileExp exp2
  return $ exp1Code ++ exp2Code ++ binCode

-- Get the local slot for the given variable name.
-- Create a new slot if the variable was not allocated before.
getOrCreateLoc :: Ident -> Compiler Loc
getOrCreateLoc ident = do
  mbloc <- gets (Map.lookup ident)
  case mbloc of
    Just loc -> return loc
    Nothing -> do
      loc <- newloc
      modify (Map.insert ident loc)
      return loc
  where
    newloc = getLocCount

-- Get the total number of local variables allocated.
getLocCount :: Compiler Int
getLocCount = gets Map.size
