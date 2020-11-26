module Llvm.Compiler
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
import           Llvm.Emit           as Emit

type Pos = Maybe (Int, Int)

type Store = Map.Map Ident RegVal

data CompilerState = St {store :: Store, regpool :: Int}

type Compiler = StateT CompilerState Err

-- A computed value consists of the code used to compute it
-- and a parameter in which the value is located afterwards.
data EmittedVal = EmittedVal {code :: [String], param :: LlvmParam}

getsStore :: (Store -> a) -> Compiler a
getsStore f = gets (f . store)

getRegpool :: Compiler Int
getRegpool = gets regpool

modifyStore :: (Store -> Store) -> Compiler ()
modifyStore f = modify (\x -> x {store = f (store x)})

modifyRegpool :: (Int -> Int) -> Compiler ()
modifyRegpool f = modify (\x -> x {regpool = f (regpool x)})

-- Turn code and a register into an EmittedVal.
emittedVal :: [String] -> RegVal -> EmittedVal
emittedVal code reg = EmittedVal code (Reg reg)

-- Create a constant EmittedVal.
emittedConst :: Int -> EmittedVal
emittedConst num = EmittedVal [] (Const num)

runCompiler :: Compiler a -> Err a
runCompiler i = evalStateT i (St Map.empty 0)

compileProgram :: Program Pos -> Compiler [String]
compileProgram (Prog _ stmts) = do
  stmtCode <- mapM compileStmt stmts
  let mainBody = concat stmtCode
      codeBlocks =
        [ Emit.printIntDefinition,
          Emit.mainHeader,
          mainBody,
          Emit.mainEnd
        ]
      code = intercalate [""] codeBlocks
  return code

compileStmt :: Stmt Pos -> Compiler [String]
compileStmt x = case x of
  SAss _ ident exp -> do
    exp' <- compileExp exp
    allocExp <- getOrAlloc ident
    let (Reg reg) = param allocExp
        assCode = Emit.storeInt reg (param exp')
    return $ code allocExp ++ code exp' ++ assCode
  SExp _ exp -> do
    exp' <- compileExp exp
    let printCode = Emit.printInt (param exp')
    return $ code exp' ++ printCode

compileExp :: Exp Pos -> Compiler EmittedVal
compileExp x = case x of
  ExpAdd _ exp1 exp2 -> emitBin Emit.addInts exp1 exp2
  ExpSub _ exp1 exp2 -> emitBin Emit.subInts exp1 exp2
  ExpMul _ exp1 exp2 -> emitBin Emit.mulInts exp1 exp2
  ExpDiv _ exp1 exp2 -> emitBin Emit.divInts exp1 exp2
  ExpVar a ident -> do
    mbreg <- getsStore (Map.lookup ident)
    case mbreg of
      Just src -> do
        reg <- newreg
        let code = Emit.loadInt reg src
        return $ emittedVal code reg
      Nothing -> fail $ "Unassigned variable '" ++ show ident ++ "' at " ++ show a
  ExpLit _ num -> return $ emittedConst (fromInteger num)

type BinEmit = RegVal -> LlvmParam -> LlvmParam -> [String]

emitBin :: BinEmit -> Exp Pos -> Exp Pos -> Compiler EmittedVal
emitBin emit exp1 exp2 = do
  exp1' <- compileExp exp1
  exp2' <- compileExp exp2
  reg <- newreg
  let binCode = emit reg (param exp1') (param exp2')
  return $ emittedVal (code exp1' ++ code exp2' ++ binCode) reg

-- Retrieve the address of a local under a given Ident.
-- If it was not allocated before, create a new local.
getOrAlloc :: Ident -> Compiler EmittedVal
getOrAlloc ident = do
  mbreg <- getsStore (Map.lookup ident)
  case mbreg of
    Just reg -> return $ emittedVal [] reg
    Nothing -> do
      reg <- newreg
      modifyStore (Map.insert ident reg)
      return $ emittedVal (Emit.allocInt reg) reg

newreg :: Compiler RegVal
newreg = do
  n <- getRegpool
  modifyRegpool (+ 1)
  return $ RegVal n
