{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Shmonad.Expression
  ( module Control.Monad.Shmonad.Expression
  , module Control.Monad.Shmonad.Expression.Quote
  , module Control.Monad.Shmonad.Expression.Types
  ) where

import Control.Exception (assert)
import Prelude hiding ((++))
import Data.Monoid
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import System.Posix (Fd(..))
import System.Posix.IO (stdOutput, stdError)
import Control.Monad.Shmonad.Expression.Quote
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

instance ShEq a => ShEq (Quoted a) where
  (.==.) = StrEquals

instance ShEq StrSum where
  (.==.) = StrEquals

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
  = FdToFile Fd (Expr Path)
  | FdAppendToFile Fd (Expr Path)
  | FdToFd Fd Fd
  | StdinFromFile (Expr Path)

data Cmd a = Cmd
  { cmdPath :: Expr Path
  , cmdArgs :: [Expr StrSum]
  , cmdRedirs :: [Redirect]
  }

-- | A shell expression.
data Expr a where
  Lit    :: (ShShow a) => a -> Expr a
  Var    :: (Variable a) => VarID a -> Expr a
  EnvVar :: (Variable a) => VarID a -> Expr (Maybe a)
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: (ShShow a, ShShow b) => Expr a -> Expr b -> Expr StrSum
  Shown  :: (Show a) => Expr a -> Expr StrSum
  MkCmd  :: Cmd a -> Expr (Cmd a)
  And    :: Expr (Cmd a) -> Expr (Cmd b) -> Expr (Cmd c)
  Or     :: Expr (Cmd a) -> Expr (Cmd b) -> Expr (Cmd c)
  Pipe   :: Expr (Cmd a) -> Expr (Cmd b) -> Expr (Cmd c)
  Output :: Expr (Cmd a) -> Expr Str
  ExitC  :: Expr (Cmd a) -> Expr ShBool
  Not    :: Expr ShBool -> Expr ShBool
  StrEquals :: (ShShow a, ShShow b) => Expr a -> Expr b -> Expr ShBool
  NumCompare :: CompareOp -> Expr Integer -> Expr Integer -> Expr ShBool

instance Num (Expr Integer) where
  fromInteger = Lit
  (+)         = Plus
  negate = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined

instance IsString (Expr Str) where
  fromString = Lit . L.pack

instance IsString (Expr (Quoted Str)) where
  fromString = Lit . quoteStr . L.pack

instance IsString (Expr StrSum) where
  fromString = Lit . StrSum . L.pack

data StrSum = forall a. ShShow a => StrSum { getCat :: a }

instance Show StrSum where
  show (StrSum x) = show x

instance Variable StrSum

instance Monoid StrSum where
  mempty = StrSum ("" :: Str)
  StrSum x `mappend` StrSum y = StrSum $ toShString x <> toShString y

instance ShShow StrSum where
  toShString (StrSum x) = toShString x

instance Monoid (Expr StrSum) where
  mempty = Lit $ StrSum ("" :: L.Text)
  mappend = Concat

path :: Str -> Expr Path
path = Lit . Path . L.unpack

str :: (ShShow a) => Expr a -> Expr StrSum
str = Shown

quote :: Str -> Expr StrSum
quote = Lit . StrSum . quoteStr

-- * Redirects

redirToStr :: Redirect -> Expr StrSum
redirToStr r
  = case r of
      FdToFile fd@(Fd n) p -> assert (n > 0) fdToFile "> " fd (toInteger n) p
      FdAppendToFile fd@(Fd n) p -> assert (n > 0) fdToFile ">> " fd (toInteger n) p
      FdToFd (Fd a) (Fd b) -> assert (a > 0 && b > 0) Shown (Lit $ toInteger a) <> ">&" <> Shown (Lit $ toInteger b)
      StdinFromFile p -> "< " <> Shown p
    where fdToFile x fd n p 
            = case () of _
                          | fd == stdOutput -> x <> Shown p
                          | otherwise -> Shown (Lit n) <> x <> Shown p

toFile :: Expr Path -> Redirect
toFile = FdToFile stdOutput

appendToFile :: Expr Path -> Redirect
appendToFile = FdAppendToFile stdOutput

stderrToStdout :: Redirect
stderrToStdout = FdToFd stdError stdOutput

fromFile :: Expr Path -> Redirect
fromFile = StdinFromFile

space :: [L.Text] -> L.Text 
space = L.intercalate " "

andThen :: Expr (Cmd a) -> Expr (Cmd b) -> Expr (Cmd c)
andThen = And

orElse :: Expr (Cmd a) -> Expr (Cmd b) -> Expr (Cmd c)
orElse = Or

pipe :: Expr (Cmd a) -> Expr (Cmd b) -> Expr (Cmd c)
pipe = Pipe

varFromEnv :: Variable a => Name -> Expr (Maybe a)
varFromEnv n = Var (VarID Nothing n)

shCompare :: Expr a -> Str -> Expr a -> Str
shCompare e1 c e2 = "[ " <> shExpr e1 <> " " <> c <> " " <> shExpr e2 <> " ]"

-- | Returns the text of an expression
shExpr :: Expr a -> Str
shExpr expr = case expr of
  Lit x                    -> toShString x
  Var x                    -> "${" <> fromName (uniqueName x) <> "}"
  EnvVar x                 -> "${" <> fromName (uniqueName x) <> ":-}"
  Plus expr1 expr2         -> "$((" <> shExpr expr1 <> "+" <> shExpr expr2 <> "))"
  Concat expr1 expr2       -> shExpr expr1 <> shExpr expr2
  Shown expr'              -> shExpr expr'
  MkCmd c                  -> shExpr (cmdPath c)
                                <> space (map shExpr (cmdArgs c))
                                <> space (map (shExpr . redirToStr) (cmdRedirs c))
  And c1 c2                -> shExpr c1 <> " && " <> shExpr c2
  Or c1 c2                 -> shExpr c1 <> " || " <> shExpr c2
  Pipe c1 c2               -> shExpr c1 <> " | " <> shExpr c2
  Output e                 -> "$(" <> shExpr e <> ")"
  ExitC e                  -> shExpr e
  Not expr'                -> "! " <> shExpr expr'
  StrEquals expr1 expr2    -> shCompare (Lit $ shExpr expr1) "=" (Lit $shExpr expr2)
  NumCompare c expr1 expr2 -> case c of 
                                Equal -> shCompare expr1 "-eq" expr2
                                LessThan -> shCompare expr1 "-lt" expr2
                                LessOrEqual -> shCompare expr1 "-le" expr2
                                GreaterThan -> shCompare expr1 "-gt" expr2
                                GreaterOrEqual -> shCompare expr1 "-ge" expr2
