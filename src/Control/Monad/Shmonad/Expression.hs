{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Shmonad.Expression
  ( UniqueID
  , Str
  , Name
  , isValidName
  , toName
  , fromName
  , VarID(..)
  , Variable(..)
  , Path
  , path
  , ShEq(..)
  , ShOrd(..)
  , ShBool(..)
  , Expr(..)
  , shExpr
  ) where

import Data.Char
import Data.String (IsString, fromString)
import Data.Monoid
import Data.Number.Nat
import System.FilePath
import qualified Data.Text.Lazy as L

default (L.Text)

type Str = L.Text

newtype Name = Name Str
  deriving (Eq, Show)

isValidName :: Str -> Bool
isValidName x = nonEmpty && allValidChars && notStartWithNumber
  where nonEmpty = not (L.null x)
        allValidChars = L.all (\c -> isAlphaNum c || c == '_') x
        notStartWithNumber = not . isDigit $ L.head x 

toName :: Str -> Name
toName x
  | isValidName x = Name x
  | otherwise = error $ L.unpack ("\"" <> x <> "\" is not a valid shell name.")

fromName :: Name -> Str
fromName (Name x) = x

instance IsString Name where
  fromString = toName . L.pack

type UniqueID = Nat

data VarID a = VarID
  { varID   :: UniqueID
  , varName :: Name
  } deriving (Eq, Show)

-- | A Variable provides a unique name for reference in the shell.
class (Show a, Eq a) => Variable a where
  uniqueName :: VarID a -> Name 
  uniqueName (VarID vID vName) = toName $ fromName vName <> L.pack (show vID)

instance Variable Str
instance Variable Integer

newtype Path = Path FilePath
  deriving (Eq, Show)

path :: Str -> Path
path = Path . makeValid . L.unpack

instance Variable Path

newtype ShBool = ShBool Bool
  deriving Eq

instance Show ShBool where
  show (ShBool b) = if b then "(true)" else "(false)"

instance Variable ShBool

class (Variable a) => ShEq a where
  (.==.) :: Expr a -> Expr a -> Expr ShBool

  (./=.) :: Expr a -> Expr a -> Expr ShBool
  x ./=. y = Not (x .==. y)

instance ShEq Str where
  (.==.) = StrEquals

instance ShEq Integer where
  (.==.) = NumCompare Equal

data CompareOp = Equal | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual
  deriving Show

class (Variable a) => ShOrd a where
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
