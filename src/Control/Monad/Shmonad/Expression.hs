{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.Expression where

import Data.String (IsString, fromString)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L

type UniqueID = Integer
type ExitValue = Integer
type Str = L.Text
type Name = Str

data VarID a = VarID
  { varID   :: UniqueID
  , varName :: Str
  } deriving (Eq, Show)

-- | A Variable provides a unique name for reference in the shell.
class (Show a) => Variable a where
  uniqueName :: VarID a -> Str 
  uniqueName (VarID vID vName) = vName <> L.pack (show vID)

instance Variable Str
instance Variable Integer

newtype Path = Path FilePath
  deriving (Eq, Show)

instance Variable Path

-- | A shell expression.
data Expr a where
  Lit    :: (Variable a) => a -> Expr a 
  Var    :: (Variable a) => VarID a -> Expr a
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: Expr Str -> Expr Str -> Expr Str
  Shown  :: (Variable a) => Expr a -> Expr Str
--  Test   :: Condition -> Expr ExitValue

instance Num (Expr Integer) where
  fromInteger = Lit
  (+)         = Plus
  negate = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined

instance IsString (Expr Str) where
  fromString = Lit . L.pack

-- | Concatenates two string expressions.
(++) :: Expr Str -> Expr Str -> Expr Str
(++) = Concat

-- | Returns the text of an expression
shExpr :: (Show a, Variable a) => Expr a -> Str
shExpr expr = case expr of
  Lit x               -> L.pack $ show x
  Var x               -> "${" <> uniqueName x <> "}"
  Plus expr1 expr2    -> "$((" <> shExpr expr1 <> "+" <> shExpr expr2 <> "))"
  Concat expr1 expr2  -> shExpr expr1 <> shExpr expr2
  Shown  expr'        -> shExpr expr'
