{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Shmonad where

import Control.Monad.Free
import Control.Monad
import qualified Data.Text.Lazy as L
import Data.Monoid
import Data.String
import Prelude hiding ((++))

default (L.Text)

type UniqueID = Integer
type Name = L.Text

type Str = L.Text

data VStr = VStr 
  { strID :: UniqueID
  , strName :: Name }

data VInt = VInt 
  { intID :: UniqueID
  , intName :: Name }

class Named a where
  name :: a -> Name

instance Named VStr where
  name (VStr nID n) = n <> L.pack (show nID)

instance Named VInt where
  name (VInt nID n) = n <> L.pack (show nID)

data Expr a where
  StrL   :: Str  -> Expr Str        -- String  literal
  IntL   :: Integer -> Expr Integer -- Integer literal
  StrV   :: VStr    -> Expr Str     -- String  variable
  IntV   :: VInt    -> Expr Integer -- Integer variable
  Plus   :: Expr Integer -> Expr Integer -> Expr Integer
  Concat :: Expr Str  -> Expr Str  -> Expr Str
  Shown  :: Expr Integer -> Expr Str

instance Num (Expr Integer) where
  fromInteger = IntL
  (+)         = Plus
  negate = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined

instance IsString (Expr Str) where
    fromString = StrL . L.pack

(++) :: Expr Str -> Expr Str -> Expr Str
(++) = Concat

data ScriptF next
    = NewInt Name (Expr Integer) (VInt -> next)
    | NewStr Name (Expr Str) (VStr -> next)
    | SetInt VInt (Expr Integer) next
    | SetStr VStr (Expr Str) next
    | Echo (Expr Str) next
    | Exit (Expr Integer)
  deriving (Functor)

type Script = Free ScriptF

newInt :: Name -> Expr Integer -> Script VInt
newInt s n = liftF $ NewInt s n id

newStr :: Name -> Expr Str -> Script VStr
newStr s str = liftF $ NewStr s str id

setStr :: VStr -> Expr Str -> Script ()
setStr v expr = liftF $ SetStr v expr ()

setInt :: VInt -> Expr Integer -> Script ()
setInt v expr = liftF $ SetInt v expr ()

echo :: Expr Str -> Script ()
echo expr = liftF $ Echo expr ()

exit :: Expr Integer -> Script r
exit expr = liftF $ Exit expr

script1 :: Script r
script1 = do
    hello <- newStr "hello" "Hello, "
    world <- newStr "world" "World!"
    setStr hello (StrV hello ++ StrV world)
    echo ("hello: " ++ StrV hello)
    echo ("world: " ++ StrV world)
    x <- newInt "x" 4
    y <- newInt "y" 5
    exit (IntV x + IntV y)

script2 :: Script ()
script2 = forM_ [1..5] $ \i -> do
    x <- newInt "x" (IntL i)
    setInt x (IntV x + 5)
    echo (Shown (IntV x))

bashExpr :: Expr a -> Str
bashExpr expr = case expr of
    StrL str           -> str
    IntL int           -> L.pack $ show int
    StrV x             -> "${" <> name x <> "}"
    IntV x             -> "${" <> name x <> "}"
    Plus   expr1 expr2 ->
      "$((" <> bashExpr expr1 <> "+" <> bashExpr expr2 <> "))"
    Concat expr1 expr2 -> bashExpr expr1 <> bashExpr expr2
    Shown  expr'       -> bashExpr expr'


bashBackend :: Script r -> Str
bashBackend = go 0 where
  newVarS v e = name v <> "=" <> bashExpr e <> "\n"
  go n script =
    case script of
      Free f -> case f of
        NewInt s e k ->
          let v = VInt n s
          in newVarS v e <> go (n + 1) (k v)
        NewStr s e k ->
          let v = VStr n s
          in newVarS v e <> go (n + 1) (k v)
        SetStr v e script' ->
            name v <> "=" <> bashExpr e <> "\n" <>
              go n script'
        SetInt v e script' ->
            name v <> "=" <> bashExpr e <> "\n" <>
              go n script'
        Echo e script' ->
            "echo " <> bashExpr e <> "\n" <>
                go n script'
        Exit e ->
            "exit " <> bashExpr e <> "\n"
      Pure _ -> ""
