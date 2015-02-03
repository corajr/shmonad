{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Shmonad.Expression
  ( module Control.Monad.Shmonad.Expression
  , module X
  ) where

import Control.Exception (assert)
import Prelude hiding ((++))
import Data.Monoid
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import System.Posix (Fd(..))
import System.Posix.IO (stdOutput, stdError)
import Control.Monad.Shmonad.Expression.Quote as X
import Control.Monad.Shmonad.Expression.Test as X
import Control.Monad.Shmonad.Expression.Types as X

default (L.Text)

-- | Two Variables may be compared for equality.
class (Variable a) => ShEq a where
  (.==.) :: Expr a -> Expr a -> Expr ShBool

  (./=.) :: Expr a -> Expr a -> Expr ShBool
  x ./=. y = Not (x .==. y)

instance ShEq Str where
  (.==.) = StrEquals

instance ShEq Integer where
  (.==.) = BinaryTest Equal

instance ShEq a => ShEq (Quoted a) where
  (.==.) = StrEquals

instance ShEq StrSum where
  (.==.) = StrEquals

-- | Two numeric variables may be less than, greater than, or equal to each other.
class (Variable a, Num a) => ShOrd a where
  (.>.) :: Expr a -> Expr a -> Expr ShBool
  (.>=.) :: Expr a -> Expr a -> Expr ShBool
  (.<.) :: Expr a -> Expr a -> Expr ShBool
  (.<=.) :: Expr a -> Expr a -> Expr ShBool

instance ShOrd Integer where
  (.>.)  = BinaryTest GreaterThan
  (.>=.) = BinaryTest GreaterOrEqual
  (.<.)  = BinaryTest LessThan
  (.<=.) = BinaryTest LessOrEqual

data Redirect
  = FdToFile Fd (Expr Path)
  | FdAppendToFile Fd (Expr Path)
  | FdToFd Fd Fd
  | StdinFromFile (Expr Path)

data Cmd a i o = Cmd
  { cmdName :: Expr StrSum
  , cmdArgs :: [Expr StrSum]
  , cmdRedirs :: [Redirect]
  }

-- | A shell expression.
data Expr a where
  Lit    :: (ShShow a) => a -> Expr a
  Var    :: (Variable a) => VarID a -> Expr a
  MaybeV  :: (Variable a) => VarID a -> Expr (Maybe a)
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: (ShShow a, ShShow b) => Expr a -> Expr b -> Expr StrSum
  Shown  :: (Show a) => Expr a -> Expr StrSum
  MkCmd  :: Cmd a i o -> Expr (Cmd a i o)
  And    :: Expr (Cmd a i o) -> Expr (Cmd b i1 o1) -> Expr (Cmd c i2 o2)
  Or     :: Expr (Cmd a i o) -> Expr (Cmd b i1 o1) -> Expr (Cmd c i2 o2)
  Pipe   :: Expr (Cmd a i o) -> Expr (Cmd b o o1) -> Expr (Cmd c o1 o2)
  Output :: Expr (Cmd a i o) -> Expr o
  ExitC  :: Expr (Cmd a i o) -> Expr ShBool
  Not    :: Expr ShBool -> Expr ShBool
  UnaryTest :: (ShTest a) => Test a -> Expr a -> Expr ShBool
  BinaryTest :: (ShTest a) => Test a -> Expr a -> Expr a -> Expr ShBool
  StrEquals :: (ShShow a, ShShow b) => Expr a -> Expr b -> Expr ShBool

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

instance Monoid (Expr StrSum) where
  mempty = Lit $ StrSum ("" :: L.Text)
  mappend = Concat

path :: Str -> Expr Path
path = Lit . Path . L.unpack

str :: Str -> Expr StrSum
str = Lit . StrSum

shShow :: (ShShow a) => Expr a -> Expr StrSum
shShow = Shown

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
space [] = ""
space xs = L.cons ' ' $ L.intercalate " " xs

andThen :: Expr (Cmd a i o) -> Expr (Cmd b i1 o1) -> Expr (Cmd c i2 o2)
andThen = And

(.&&.) :: Expr (Cmd a i o) -> Expr (Cmd b i1 o1) -> Expr (Cmd c i2 o2)
(.&&.) = andThen

orElse :: Expr (Cmd a i o) -> Expr (Cmd b i1 o1) -> Expr (Cmd c i2 o2)
orElse = Or

(.||.) :: Expr (Cmd a i o) -> Expr (Cmd b i1 o1) -> Expr (Cmd c i2 o2)
(.||.) = orElse

pipe :: Expr (Cmd a i o) -> Expr (Cmd b o o1) -> Expr (Cmd c o1 o2)
pipe = Pipe

(.|.) :: Expr (Cmd a i o) -> Expr (Cmd b o o1) -> Expr (Cmd c o1 o2)
(.|.) = pipe

test :: (ShTest a) => Test a -> Expr a -> Expr ShBool
test = UnaryTest

varFromEnvUnsafe :: Variable a => Name -> Expr a
varFromEnvUnsafe n = Var (VarID Nothing n)

shUnary :: Str -> Expr a -> Str
shUnary s e = s <> " " <> shExpr e

shCompare :: Str -> Expr a -> Expr b -> Str
shCompare c e1 e2 = "[ " <> shExpr e1 <> " " <> c <> " " <> shExpr e2 <> " ]"

-- | Returns the text of an expression
shExpr :: Expr a -> Str
shExpr expr = case expr of
  Lit x                    -> toShString x
  Var x                    -> "${" <> fromName (uniqueName x) <> "}"
  MaybeV x                 -> "${" <> fromName (uniqueName x) <> ":-}"
  Plus expr1 expr2         -> "$((" <> shExpr expr1 <> "+" <> shExpr expr2 <> "))"
  Concat expr1 expr2       -> shExpr expr1 <> shExpr expr2
  Shown expr'              -> shExpr expr'
  MkCmd c                  -> shExpr (cmdName c)
                                <> space (map shExpr (cmdArgs c))
                                <> space (map (shExpr . redirToStr) (cmdRedirs c))
  And c1 c2                -> shExpr c1 <> " && " <> shExpr c2
  Or c1 c2                 -> shExpr c1 <> " || " <> shExpr c2
  Pipe c1 c2               -> shExpr c1 <> " | " <> shExpr c2
  Output e                 -> "$(" <> shExpr e <> ")"
  ExitC e                  -> shExpr e
  Not e                    -> shUnary "!" e
  UnaryTest t e            -> shUnary (toTestStr t) e
  BinaryTest c expr1 expr2 -> shCompare (toTestStr c) expr1 expr2
  StrEquals expr1 expr2    -> shCompare "=" expr1 expr2
