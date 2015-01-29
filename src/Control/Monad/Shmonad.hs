{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Control.Monad.Shmonad
  ( module Control.Monad.Shmonad
  , module Control.Monad.Shmonad.Expression
  , module Control.Monad.Shmonad.Statement
  ) where

import Prelude hiding ((++))
-- import Control.Monad
import Control.Monad.Free
import Control.Monad.RWS.Lazy
import qualified Data.Text.Lazy as L
import Data.Number.Nat

import Control.Monad.Shmonad.Expression
import Control.Monad.Shmonad.Statement

default (L.Text)

-- | Create a new variable with given name and value. Once created,
-- a variable must always take values of the same type.
newVar :: (Variable a) => Name -> Expr a -> Script (VarID a)
newVar name val = liftF $ NewVar name val id

-- | Set the variable's value.
setVar :: (Variable a) => VarID a -> Expr a -> Script ()
setVar v expr = liftF $ SetVar v expr ()

-- | Echo a string.
echo :: Expr Str -> Script ()
echo expr = liftF $ Echo expr ()

-- cmd :: Expr Path -> [Expr Str] -> [Redirect] -> Script (Expr ExitValue)
-- cmd p args' redirs' = liftF $ Command (Cmd p args' redirs') id

-- | Exit with a result code.
exit :: Expr Integer -> Script ()
exit expr = liftF $ Exit expr ()

-- | Transpiles the DSL into shell script.
toShellScript :: Script next -> Str
toShellScript script = w
  where (_, _, w) = runRWS (transpile script) () (0 :: Nat)
