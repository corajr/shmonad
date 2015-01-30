{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.Expression
  ( module Control.Monad.Shmonad.Expression
  , module Control.Monad.Shmonad.Expression.Types
  ) where

import Data.Monoid
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L

import Control.Monad.Shmonad.Expression.Types

default (L.Text)

-- | Two Variables may be compared for equality.
class (Variable a) => ShEq a where
  (.==.) :: Expr a -> Expr a -> Expr ShBool

  (./=.) :: Expr a -> Expr a -> Expr ShBool
  x ./=. y = Not (x .==. y)

instance ShEq Str where
  (.==.) = StrEquals

instance ShEq Integer where
  (.==.) = NumCompare Equal

-- | Enumeration of numerical comparisons (==, <, >, ...).
data CompareOp = Equal | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual
  deriving Show

-- | Two numeric variables may be less than, greater than, or equal to each other.
class (Variable a, Num a) => ShOrd a where
  (.>.) :: Expr a -> Expr a -> Expr ShBool
  (.>=.) :: Expr a -> Expr a -> Expr ShBool
  (.<.) :: Expr a -> Expr a -> Expr ShBool
  (.<=.) :: Expr a -> Expr a -> Expr ShBool

instance ShOrd Integer where
  (.>.)  = NumCompare GreaterThan
  (.>=.) = NumCompare GreaterOrEqual
  (.<.)  = NumCompare LessThan
  (.<=.) = NumCompare LessOrEqual

-- | A shell expression.
data Expr a where
  Lit    :: (Variable a) => a -> Expr a 
  Var    :: (Variable a) => VarID a -> Expr a
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: Expr Str -> Expr Str -> Expr Str
  Shown  :: (Variable a) => Expr a -> Expr Str
  Not    :: Expr ShBool -> Expr ShBool
  StrEquals :: Expr Str -> Expr Str -> Expr ShBool
  NumCompare :: CompareOp -> Expr Integer -> Expr Integer -> Expr ShBool

deriving instance Variable a => Show (Expr a)

instance Num (Expr Integer) where
  fromInteger = Lit
  (+)         = Plus
  negate = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined

instance IsString (Expr Str) where
  fromString = Lit . L.pack

instance Monoid (Expr Str) where
  mempty = Lit ""
  mappend = Concat

-- | Returns the text of an expression
shExpr :: (Show a, Variable a) => Expr a -> Str
shExpr expr = case expr of
  Lit x                    -> L.pack $ show x
  Var x                    -> "${" <> fromName (uniqueName x) <> "}"
  Plus expr1 expr2         -> "$((" <> shExpr expr1 <> "+" <> shExpr expr2 <> "))"
  Concat expr1 expr2       -> shExpr expr1 <> shExpr expr2
  Shown expr'              -> shExpr expr'
  Not expr'                -> "! " <> shExpr expr'
  StrEquals expr1 expr2    -> "[ " <> shExpr expr1 <> " = " <> shExpr expr2 <> " ]"
  NumCompare c expr1 expr2 -> case c of 
                                Equal -> "[ " <> shExpr expr1 <> " -eq " <> shExpr expr2 <> " ]"
                                LessThan -> "[ " <> shExpr expr1 <> " -lt " <> shExpr expr2 <> " ]"
                                LessOrEqual -> "[ " <> shExpr expr1 <> " -le " <> shExpr expr2 <> " ]"
                                GreaterThan -> "[ " <> shExpr expr1 <> " -gt " <> shExpr expr2 <> " ]"
                                GreaterOrEqual -> "[ " <> shExpr expr1 <> " -ge " <> shExpr expr2 <> " ]"
