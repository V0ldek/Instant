module Jvm.Abs where

import           AbsInstant
import           Data.List  (foldl')

type Pos = Maybe (Int, Int)

data AbsMetadata = AbsMetadata {pos :: Pos, stackLimit :: Int} deriving (Show)

annotate :: Program Pos -> Program AbsMetadata
annotate (Prog a stmts) =
  let stmts' = map annotateStmt stmts
      limit = foldl' max 0 $ map (stackLimit . ann) stmts'
   in Prog (AbsMetadata a limit) stmts'

annotateStmt :: Stmt Pos -> Stmt AbsMetadata
annotateStmt (SAss a ident exp) =
  let exp' = annotateExp exp
   in SAss ((ann exp') {pos = a}) ident exp'
annotateStmt (SExp a exp) =
  let exp' = annotateExp exp
   in SExp ((ann exp') {pos = a}) exp'

annotateExp :: Exp Pos -> Exp AbsMetadata
annotateExp x = case x of
  ExpAdd a exp1 exp2 -> rebalanceAndConsBin ExpAdd a (annotateExp exp1) (annotateExp exp2)
  ExpSub a exp1 exp2 -> consBin ExpSub a (annotateExp exp1) (annotateExp exp2)
  ExpMul a exp1 exp2 -> rebalanceAndConsBin ExpMul a (annotateExp exp1) (annotateExp exp2)
  ExpDiv a exp1 exp2 -> consBin ExpDiv a (annotateExp exp1) (annotateExp exp2)
  ExpLit a num -> ExpLit (AbsMetadata a 1) num
  ExpVar a ident -> ExpVar (AbsMetadata a 1) ident

type BinCons a = (a -> Exp a -> Exp a -> Exp a)

-- Construct a tree with calculated stack limit of an operation based on its subtrees.
consBin :: BinCons AbsMetadata -> Pos -> Exp AbsMetadata -> Exp AbsMetadata -> Exp AbsMetadata
consBin c a exp1 exp2 =
  {- The stack limit is calculated as follows:
       - First the left tree is calculated in whole which takes at most its stackLimit
         and leaves one item on the stack.
       - Then we go into the right tree, where at most its stackLimit plus one for the
         left tree result is put on the stack.
  -}
  let immediateStackLimit = 1 + stackLimit (ann exp2)
      totalStackLimit = max immediateStackLimit (stackLimit $ ann exp1)
   in c (AbsMetadata a totalStackLimit) exp1 exp2

-- Commutative operations can be reordered before consBin if it improves the stack limit.
-- Based on how consBin calculation works we want the larger stack limit to be in the right subtree.
rebalanceAndConsBin :: BinCons AbsMetadata -> Pos -> Exp AbsMetadata -> Exp AbsMetadata -> Exp AbsMetadata
rebalanceAndConsBin c a exp1 exp2 =
  if stackLimit (ann exp2) > stackLimit (ann exp1)
    then consBin c a exp2 exp1
    else consBin c a exp1 exp2
