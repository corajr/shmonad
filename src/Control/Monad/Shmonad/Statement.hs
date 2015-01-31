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
  Command :: (Command a) => Expr (Cmd a) -> next -> Statement next
  Conditional :: Cond (Expr ShBool) (Statement a) -> next -> Statement next
  Echo :: Expr Str -> next -> Statement next
  Exit :: Expr Integer -> next -> Statement next

instance Boolean (Expr ShBool)
instance CondCommand (Statement next)

instance Functor Statement where
  fmap f (NewVar name' expr' cont) = NewVar name' expr' (f . cont)
  fmap f (SetVar v e n) = SetVar v e (f n)
  fmap f (Command c n) = Command c (f n)
  fmap f (Conditional c n) = Conditional c (f n)
  fmap f (Echo str n) = Echo str (f n)
  fmap f (Exit e n) = Exit e (f n)

type Script = Free Statement

indent :: Transpiler () -> Transpiler ()
indent t = do
  s <- get
  let (a', s', w') = runRWS t () s
  tell . L.unlines . map ("  " <>) $ L.lines w'
  put s'
  return a'

transpilePartCond :: PartialCond (Expr ShBool) (Statement a) -> Transpiler ()
transpilePartCond pc
  = case pc of
      Then (If b) cmd -> do
        tell $ "if " <> shExpr b <> "; then\n"
        indent $ transpile (return cmd)
      ElifThen pc' b cmd -> do
        transpilePartCond pc'
        tell $ "elif " <> shExpr b <> "; then\n"
        indent $ transpile (return cmd)

transpileCond :: Cond (Expr ShBool) (Statement a) -> Transpiler ()
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
      let vi = VarID n name' :: VarID v
      put (n + 1)
      tell $ fromName (uniqueName vi) <> "=" <> shExpr expr' <> "\n"
      transpile (cont vi)
    SetVar v e n -> do
      tell $ fromName (uniqueName v) <> "=" <> shExpr e <> "\n"
      transpile n
    Command c n -> do
      tell $ shExpr c <> "\n"
      transpile n
    Conditional c n -> do
      transpileCond c
      transpile n
    Echo str n -> do
      tell $ "echo " <> shExpr str <> "\n"
      transpile n
    Exit e n -> do
      tell $ "exit " <> shExpr e <> "\n"
      transpile n
  Pure _ -> return ()
