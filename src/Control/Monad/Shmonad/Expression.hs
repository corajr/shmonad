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
import System.Posix (Fd)
-- import System.Posix.IO (stdInput, stdOutput, stdError)
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

data Redirect
  = FdToFile Path
  | FdToFd Fd Fd
  | StdinFromFile Path
  deriving (Eq, Show)

data Cmd a = Cmd
  { cmdPath :: Expr Path
  , cmdArgs :: [Expr Str]
  , cmdRedirs :: [Redirect]
  } deriving Show

-- | A shell expression.
data Expr a where
  Lit    :: (Variable a) => a -> Expr a 
  Var    :: (Variable a) => VarID a -> Expr a
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: Expr Str -> Expr Str -> Expr Str
  Shown  :: (Show a) => Expr a -> Expr Str
  MkCmd  :: Cmd a -> Expr (Cmd a)
  Output :: Expr (Cmd a) -> Expr Str
  ExitC  :: Expr (Cmd a) -> Expr ShBool
  Not    :: Expr ShBool -> Expr ShBool
  StrEquals :: Expr Str -> Expr Str -> Expr ShBool
  NumCompare :: CompareOp -> Expr Integer -> Expr Integer -> Expr ShBool

deriving instance Show (Expr a)

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

path :: Str -> Expr Path
path = Lit . Path . L.unpack

space :: [L.Text] -> L.Text 
space = L.intercalate " "

shCompare :: Expr a -> Str -> Expr a -> Str
shCompare e1 c e2 = "[ " <> shExpr e1 <> " " <> c <> " " <> shExpr e2 <> " ]"

-- | Returns the text of an expression
shExpr :: Expr a -> Str
shExpr expr = case expr of
  Lit x                    -> L.pack $ show x
  Var x                    -> "${" <> fromName (uniqueName x) <> "}"
  Plus expr1 expr2         -> "$((" <> shExpr expr1 <> "+" <> shExpr expr2 <> "))"
  Concat expr1 expr2       -> shExpr expr1 <> shExpr expr2
  Shown expr'              -> shExpr expr'
  MkCmd c                  -> shExpr (cmdPath c)
                                <> space (map shExpr (cmdArgs c))
                                <> space (map (L.pack . show) (cmdRedirs c))
  Output e                 -> "$(" <> shExpr e <> ")"
  ExitC e                  -> shExpr e
  Not expr'                -> "! " <> shExpr expr'
  StrEquals expr1 expr2    -> shCompare expr1 "=" expr2
  NumCompare c expr1 expr2 -> case c of 
                                Equal -> shCompare expr1 "-eq" expr2
                                LessThan -> shCompare expr1 "-lt" expr2
                                LessOrEqual -> shCompare expr1 "-le" expr2
                                GreaterThan -> shCompare expr1 "-gt" expr2
                                GreaterOrEqual -> shCompare expr1 "-ge" expr2
