{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Control.Monad.Shmonad.Statement where

import Control.Monad.Free
import Control.Monad.RWS.Lazy
import Control.Monad.Shmonad.Command
import Control.Monad.Shmonad.Conditional
import Control.Monad.Shmonad.Expression
import qualified Data.Text.Lazy as L
import Data.Number.Nat

default (L.Text)

-- | Provides a writer and state monad for transforming ASTs into shell script.
type Transpiler = RWS () Str Nat

data Statement next where
  NewVar :: (Variable v) => Name -> Expr v -> (VarID v -> next) -> Statement next
  SetVar :: (Variable v) => VarID v -> Expr v -> next -> Statement next
  EnvVar :: (Variable v, ShShow v) => Expr (Maybe v) -> Expr StrSum -> next -> Statement next
  RunCommand :: Expr (Cmd a) -> next -> Statement next
  Conditional :: Cond (Expr ShBool) (Script a) -> next -> Statement next

instance Boolean (Expr ShBool)
instance CondCommand (Script next)

instance Functor Statement where
  fmap f (NewVar name' expr' cont) = NewVar name' expr' (f . cont)
  fmap f (SetVar v e n) = SetVar v e (f n)
  fmap f (EnvVar mayv errmsg n) = EnvVar mayv errmsg (f n)
  fmap f (RunCommand c n) = RunCommand c (f n)
  fmap f (Conditional c n) = Conditional c (f n)

type Script = Free Statement

-- | Create a new variable with given name and value. Once created,
-- a variable must always take values of the same type.
newVar :: (Variable a) => Name -> Expr a -> Script (VarID a)
newVar name val = liftF $ NewVar name val id

-- | Set the variable's value.
setVar :: (Variable a) => VarID a -> Expr a -> Script ()
setVar v expr = liftF $ SetVar v expr ()

cmd :: Expr Path -> [Expr StrSum] -> [Redirect] -> Script ()
cmd p args' redirs' = liftF $ RunCommand (cmd' p args' redirs') ()

exec :: Expr (Cmd a) -> Script ()
exec c = liftF $ RunCommand c ()

cond :: Cond (Expr ShBool) (Script a) -> Script ()
cond c = liftF $ Conditional c ()

indent :: Transpiler () -> Transpiler ()
indent t = do
  s <- get
  let (a', s', w') = runRWS t () s
  tell . L.unlines . map ("  " <>) $ L.lines w'
  put s'
  return a'

exitErr :: (ShShow a) => Expr a -> Script ()
exitErr errMsg = exec $ echo errMsg .&&. exit 1

notExist :: (Variable v, ShShow v) => Expr (Maybe v) -> Expr StrSum -> Script ()
notExist v errMsg =
  cond $ ifThen (test ZeroLength (shShow v)) (exitErr errMsg) fi

transpilePartCond :: PartialCond (Expr ShBool) (Script a) -> Transpiler ()
transpilePartCond pc
  = case pc of
      Then (If b) c -> do
        tell $ "if " <> shExpr b <> "; then\n"
        indent $ transpile c
      ElifThen pc' b c -> do
        transpilePartCond pc'
        tell $ "elif " <> shExpr b <> "; then\n"
        indent $ transpile c

transpileCond :: Cond (Expr ShBool) (Script a) -> Transpiler ()
transpileCond c
  = case c of
      ElseFi p elseCmd -> do
        transpilePartCond p
        tell "else\n"
        indent $ transpile (return elseCmd)
        tell "fi\n"
      Fi p -> do 
        transpilePartCond p
        tell "fi\n"

transpile :: Script a -> Transpiler ()
transpile s = case s of
  Free f -> case f of
    NewVar name' expr' cont -> do
      n <- get
      let vi = VarID (Just n) name' :: VarID v
      put (n + 1)
      tell $ fromName (uniqueName vi) <> "=" <> shExpr expr' <> "\n"
      transpile (cont vi)
    SetVar v e n -> do
      tell $ fromName (uniqueName v) <> "=" <> shExpr e <> "\n"
      transpile n
    EnvVar v errmsg n -> do
      transpile (notExist v errmsg)
      transpile n
    RunCommand c n -> do
      tell $ shExpr c <> "\n"
      transpile n
    Conditional c n -> do
      transpileCond c
      transpile n
  Pure _ -> return ()
