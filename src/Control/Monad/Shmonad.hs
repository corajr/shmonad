{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Control.Monad.Shmonad
  ( module Control.Monad.Shmonad
  , module X
  ) where

import Prelude hiding ((++))
-- import Control.Monad
import Control.Monad.Free
import Control.Monad.RWS.Lazy
import qualified Data.Text.Lazy as L
import Data.Number.Nat

import Control.Monad.Shmonad.Command as X
import Control.Monad.Shmonad.Conditional as X
import Control.Monad.Shmonad.Expression as X
import Control.Monad.Shmonad.Statement as X

default (L.Text)

-- | Transpiles the DSL into shell script.
toShellScript :: Script next -> Str
toShellScript script = w
  where (_, _, w) = runRWS (transpile script) () (0 :: Nat)
