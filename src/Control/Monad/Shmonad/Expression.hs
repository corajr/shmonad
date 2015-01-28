{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Control.Monad.Shmonad.Expression
  ( UniqueID
  , ExitValue
  , Str
  , Name
  , isValidName
  , toName
  , fromName
  , VarID(..)
  , Variable(..)
  , Path
  , path
  , Expr(..)
  , shExpr
  ) where

import Data.Char
import Data.String (IsString, fromString)
import Data.Monoid
import System.FilePath
import qualified Data.Text.Lazy as L

default (L.Text)

type UniqueID = Integer
type ExitValue = Integer
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

data VarID a = VarID
  { varID   :: UniqueID
  , varName :: Name
  } deriving (Eq, Show)

-- | A Variable provides a unique name for reference in the shell.
class (Show a) => Variable a where
  uniqueName :: VarID a -> Name 
  uniqueName (VarID vID vName) = toName $ fromName vName <> L.pack (show vID)

instance Variable Str
instance Variable Integer

newtype Path = Path FilePath
  deriving (Eq, Show)

path :: Str -> Path
path = Path . makeValid . L.unpack

instance Variable Path

-- | A shell expression.
data Expr a where
  Lit    :: (Variable a) => a -> Expr a 
  Var    :: (Variable a) => VarID a -> Expr a
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: Expr Str -> Expr Str -> Expr Str
  Shown  :: (Variable a) => Expr a -> Expr Str

deriving instance Variable a => Show (Expr a)
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

instance Monoid (Expr Str) where
  mempty = Lit ""
  mappend = Concat

-- | Returns the text of an expression
shExpr :: (Show a, Variable a) => Expr a -> Str
shExpr expr = case expr of
  Lit x               -> L.pack $ show x
  Var x               -> "${" <> fromName (uniqueName x) <> "}"
  Plus expr1 expr2    -> "$((" <> shExpr expr1 <> "+" <> shExpr expr2 <> "))"
  Concat expr1 expr2  -> shExpr expr1 <> shExpr expr2
  Shown  expr'        -> shExpr expr'
