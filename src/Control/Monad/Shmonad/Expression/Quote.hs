{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Monad.Shmonad.Expression.Quote where

import Control.Monad.Shmonad.Expression.Types
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L

default (L.Text)

data Quoted a = (Variable a, Show a, ShShow a) => Quoted a

deriving instance Show (Quoted a)

instance Variable (Quoted a)

instance ShShow (Quoted a) where
  toShString (Quoted x) = toShString x

quoteStr :: Str -> Quoted Str
quoteStr = Quoted . doQuote
  where doQuote x = "\"" <> x <> "\""
