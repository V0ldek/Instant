module AbsInstant where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)

data Program a = Prog a [Stmt a]
  deriving (Eq, Ord, Show, Read)

class Annotated f where
  ann :: f a -> a

instance Functor Program where
  fmap f x = case x of
    Prog a stmts -> Prog (f a) (map (fmap f) stmts)

instance Annotated Program where
  ann (Prog a _) = a

data Stmt a = SAss a Ident (Exp a) | SExp a (Exp a)
  deriving (Eq, Ord, Show, Read)

instance Functor Stmt where
  fmap f x = case x of
    SAss a ident exp -> SAss (f a) ident (fmap f exp)
    SExp a exp       -> SExp (f a) (fmap f exp)

instance Annotated Stmt where
  ann x = case x of
    SAss a _ _ -> a
    SExp a _   -> a

data Exp a
  = ExpAdd a (Exp a) (Exp a)
  | ExpSub a (Exp a) (Exp a)
  | ExpMul a (Exp a) (Exp a)
  | ExpDiv a (Exp a) (Exp a)
  | ExpLit a Integer
  | ExpVar a Ident
  deriving (Eq, Ord, Show, Read)

instance Functor Exp where
  fmap f x = case x of
    ExpAdd a exp1 exp2 -> ExpAdd (f a) (fmap f exp1) (fmap f exp2)
    ExpSub a exp1 exp2 -> ExpSub (f a) (fmap f exp1) (fmap f exp2)
    ExpMul a exp1 exp2 -> ExpMul (f a) (fmap f exp1) (fmap f exp2)
    ExpDiv a exp1 exp2 -> ExpDiv (f a) (fmap f exp1) (fmap f exp2)
    ExpLit a integer   -> ExpLit (f a) integer
    ExpVar a ident     -> ExpVar (f a) ident

instance Annotated Exp where
  ann x = case x of
    ExpAdd a _ _ -> a
    ExpSub a _ _ -> a
    ExpMul a _ _ -> a
    ExpDiv a _ _ -> a
    ExpLit a _   -> a
    ExpVar a _   -> a
