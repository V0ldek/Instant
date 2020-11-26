module Interpreter
  ( runInterpreter,
    interpretProgram,
  )
where

import           AbsInstant
import           Control.Monad.State
import           Data.Int            (Int32)
import           Data.Map            as Map
import           Data.Maybe          (catMaybes)
import           ErrM

type Number = Int32

type Store = Map.Map Ident Number

type Interpreter = StateT Store Err

runInterpreter :: Interpreter a -> Err a
runInterpreter i = evalStateT i Map.empty

interpretProgram :: Show a => Program a -> Interpreter [Number]
interpretProgram x = case x of
  Prog _ stmts -> do
    x <- mapM interpretStmt stmts
    return $ catMaybes x

interpretStmt :: Show a => Stmt a -> Interpreter (Maybe Number)
interpretStmt x = case x of
  SAss _ ident exp -> do
    x <- interpretExp exp
    modify $ Map.insert ident x
    return Nothing
  SExp _ exp -> Just <$> interpretExp exp

interpretExp :: Show a => Exp a -> Interpreter Number
interpretExp x = case x of
  ExpAdd _ exp1 exp2 -> do
    x1 <- interpretExp exp1
    x2 <- interpretExp exp2
    return $ x1 + x2
  ExpSub _ exp1 exp2 -> do
    x1 <- interpretExp exp1
    x2 <- interpretExp exp2
    return $ x1 - x2
  ExpMul _ exp1 exp2 -> do
    x1 <- interpretExp exp1
    x2 <- interpretExp exp2
    return $ x1 * x2
  ExpDiv _ exp1 exp2 -> do
    x1 <- interpretExp exp1
    x2 <- interpretExp exp2
    return $ x1 `div` x2
  ExpLit _ integer -> return $ fromInteger integer
  ExpVar a ident -> do
    mbx <- gets $ Map.lookup ident
    case mbx of
      Just x -> return x
      Nothing -> fail $ "Undefined variable: '" ++ show ident ++ "' at " ++ show a
