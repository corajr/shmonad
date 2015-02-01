{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Control.Monad.Shmonad.Command where

import Control.Monad.Shmonad.Expression
import qualified Data.Text.Lazy as L
import GHC.TypeLits
import Data.Proxy
import Data.Monoid

default (L.Text)

data Flag (a :: Symbol)
  = Flag (Proxy a) Bool
  | FlagT (Proxy a) Str

flag :: KnownSymbol a => Flag a -> [Expr StrSum]
flag (Flag p b) = [Lit . StrSum . L.pack $ symbolVal p | b]
flag (FlagT p s) = [Lit (StrSum (L.pack (symbolVal p))) <> Lit (StrSum s) | not (L.null s)]

maybeOpt :: Show a => Maybe (Expr a) -> [Expr StrSum]
maybeOpt (Just e) = [Shown e]
maybeOpt Nothing = []

class Command a where
  data Args a :: *
  defaults :: Args a
  argsToStr :: Args a -> [Expr StrSum]

  toCmd :: Expr Path -> Args a -> [Redirect] -> Cmd a
  toCmd p args redirs = Cmd
    { cmdPath = p
    , cmdArgs = argsToStr args
    , cmdRedirs = redirs }
data Ls = Ls

instance Command Ls where
  data Args Ls = LsArgs { lsShowAll :: Flag "-A"
                        , lsLong :: Flag "-L"
                        , lsPath :: Maybe (Expr Path)
                        }
  defaults = LsArgs { lsShowAll = Flag Proxy False, lsLong = Flag Proxy False, lsPath = Nothing }
  argsToStr args = concatMap ($ args) [flag . lsShowAll, flag . lsLong, maybeOpt . lsPath]

data AnyPath = ArbCommand (Expr Path)

instance Command AnyPath where
  newtype Args AnyPath = A [Expr StrSum]
  defaults = A []
  argsToStr (A xs) = xs

ls ::  Expr (Cmd Ls)
ls = MkCmd $ toCmd (path "ls") defaults []

cmd' :: Expr Path -> [Expr StrSum] -> [Redirect] -> Expr (Cmd AnyPath)
cmd' p s = MkCmd . toCmd p (A s)
