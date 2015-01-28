{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Control.Monad.Shmonad
  ( module Control.Monad.Shmonad
  , module Control.Monad.Shmonad.Expression
  ) where

import Prelude hiding ((++))
import Control.Monad.Free
--import Control.Monad()
import qualified Data.Text.Lazy as L
import Data.Monoid
import Data.String
import System.Posix (Fd)
-- import System.Posix.IO (stdInput, stdOutput, stdError)

import Control.Monad.Shmonad.Expression
-- import Control.Monad.Shmonad.Statement

default (L.Text)

data Redirect
  = FdToFile Path
  | FdToFd Fd Fd
  | StdinFromFile Path

data Cmd = Cmd
  { program :: Expr Path
  , args :: [Expr Str]
  , redirs :: [Redirect]
  }

data ScriptF next where
  NewVar :: (Variable a) => Name -> Expr a -> (VarID a -> next) -> ScriptF next
  SetVar :: (Variable a) => VarID a -> Expr a -> next -> ScriptF next
  Echo :: Expr Str -> next -> ScriptF next
  Command :: Cmd -> (Expr ExitValue -> next) -> ScriptF next
  --If :: Expr ExitValue -> Expr Cmd -> next -> ScriptF next
  Exit :: Expr Integer -> ScriptF a

instance Functor ScriptF where
  fmap f (NewVar n e vn) = NewVar n e (f . vn)
  fmap f (SetVar vi e n) = SetVar vi e (f n)
  fmap f (Echo e n) = Echo e (f n)
  fmap f (Command c n) = Command c (f . n)
  fmap _ (Exit e) = Exit e

type Script = Free ScriptF

-- | Create a new variable with given name and value. Once created,
-- a variable must always take values of the same type.

newVar :: (Variable a) => Name -> Expr a -> Script (VarID a)
newVar name val = liftF $ NewVar name val id

setVar :: (Variable a) => VarID a -> Expr a -> Script ()
setVar v expr = liftF $ SetVar v expr ()

echo :: Expr Str -> Script ()
echo expr = liftF $ Echo expr ()

cmd :: Expr Path -> [Expr Str] -> [Redirect] -> Script (Expr ExitValue)
cmd p args' redirs' = liftF $ Command (Cmd p args' redirs') id

exit :: Expr Integer -> Script ()
exit expr = liftF $ Exit expr

toShellScript :: Script next -> Str
toShellScript = go (0 :: Integer) where
  go n script =
    case script of
      Free f -> case f of
        NewVar name' expr' k -> 
          let v = VarID n name'
          in fromName (uniqueName v) <> "=" <> shExpr expr' <> "\n" <> go (n + 1) (k v)
        SetVar v e script' -> 
          fromName (uniqueName v) <> "=" <> shExpr e <> "\n" <>
            go n script'
        Echo e script' ->
          "echo " <> shExpr e <> "\n" <>
            go n script'
        Command (Cmd p args' _) k -> 
          shExpr p <> foldl (\x y -> x <> shExpr y) "" args' <> "\n" <> 
            go n (k undefined)
        Exit e ->
          "exit " <> shExpr e <> "\n"
      Pure _ -> ""
