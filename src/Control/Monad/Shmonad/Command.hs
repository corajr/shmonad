{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Control.Monad.Shmonad.Command where

import Control.Monad.Shmonad.Expression
import qualified Data.Text.Lazy as L
import GHC.TypeLits
import Data.Proxy
import Data.Monoid

default (L.Text)

data Flag (a :: Symbol)
  = Flag { flagSymbol :: Proxy a
         , flagBool :: Bool }
  | FlagT { flagSymbol :: Proxy a
          , flagValue :: Str }

flag :: KnownSymbol a => Flag a -> [Expr StrSum]
flag (Flag p b) = [Lit . StrSum . L.pack $ symbolVal p | b]
flag (FlagT p s) = [Lit (StrSum (L.pack (symbolVal p))) <> Lit (StrSum s) | not (L.null s)]

maybeOpt :: Show a => Maybe (Expr a) -> [Expr StrSum]
maybeOpt (Just e) = [Shown e]
maybeOpt Nothing = []

class CommandName a where
  toCmdName :: a -> Expr StrSum

instance CommandName (Expr Path) where
  toCmdName = Shown

instance CommandName (Expr StrSum) where
  toCmdName = id

instance CommandName Str where
  toCmdName = str

-- | Define an Args data type and a way to translate it into a list of strings. 
-- The i and o type variables define the kind of pipelines in which
-- this command can participate.
class Command cmd i o where
  data Args cmd i o :: *
  defaults :: Args cmd i o
  argsToStr :: Args cmd i o -> [Expr StrSum]

  toCmd :: CommandName p => p -> Args cmd i o -> [Redirect] -> Cmd cmd i o
  toCmd p args redirs = Cmd
    { cmdName = toCmdName p
    , cmdArgs = argsToStr args
    , cmdRedirs = redirs }

data Echo = Echo

instance Command Echo () StrSum where
  data Args Echo () StrSum = EchoArgs { echoOmitNewline :: Flag "-n"
                                      , echoStr :: Expr StrSum
                                      }
  defaults = EchoArgs { echoOmitNewline = Flag Proxy False
                      , echoStr = "" }
  argsToStr args = flag (echoOmitNewline args) <> [echoStr args]

data Exit = Exit
instance Command Exit () () where
  data Args Exit () () = ExitArgs { exitValue :: Expr Integer }
  defaults = ExitArgs 0
  argsToStr (ExitArgs v) = [shShow v]

data Ls = Ls

instance Command Ls () [StrSum] where
  data Args Ls () [StrSum] = LsArgs { lsShowAll :: Flag "-A"
                                    , lsLong :: Flag "-l"
                                    , lsPath :: Maybe (Expr Path)
                                    }
  defaults = LsArgs { lsShowAll = Flag Proxy False, lsLong = Flag Proxy False, lsPath = Nothing }
  argsToStr args = concatMap ($ args) [flag . lsShowAll, flag . lsLong, maybeOpt . lsPath]

data Cd = Cd

instance Command Cd () () where
  data Args Cd () () = CdArgs { cdPath :: Expr Path }
  defaults = CdArgs (varFromEnvUnsafe "HOME")
  argsToStr args = [shShow $ cdPath args]

data Rm = Rm

instance Command Rm () () where
  data Args Rm () () = RmArgs { rmRecursive :: Flag "-r"
                              , rmForce :: Flag "-f"
                              , rmPaths :: [Expr Path]
                              }
  defaults = RmArgs { rmRecursive = Flag Proxy False
                    , rmForce = Flag Proxy False
                    , rmPaths = [] }
  argsToStr args = concatMap ($ args) [ flag . rmRecursive
                                      , flag . rmForce
                                      , map shShow . rmPaths
                                      ]

data Tee = Tee

instance Command Tee [StrSum] [StrSum] where
  data Args Tee [StrSum] [StrSum] = TeeArgs { teeAppend :: Flag "-a"
                                            , teePaths :: [Expr Path] }
  defaults = TeeArgs { teeAppend = Flag Proxy False, teePaths = [] }
  argsToStr args = flag (teeAppend args) <> map shShow (teePaths args)

data AnyPath = ArbCommand (Expr Path)

instance Command AnyPath [StrSum] [StrSum] where
  newtype Args AnyPath [StrSum] [StrSum] = A [Expr StrSum]
  defaults = A []
  argsToStr (A xs) = xs

echo :: (ShShow a) => Expr a -> Expr (Cmd Echo () StrSum)
echo s = MkCmd $ toCmd "echo" defaults { echoStr = shShow s } []

exit :: Expr Integer -> Expr (Cmd Exit () ())
exit v = MkCmd $ toCmd "exit" defaults { exitValue = v } []

cd :: Expr Path -> Expr (Cmd Cd () ())
cd d = MkCmd $ toCmd "cd" defaults { cdPath = d } []

ls :: Expr (Cmd Ls () [StrSum])
ls = MkCmd $ toCmd (path "ls") defaults []

rm :: Args Rm () () -> Expr (Cmd Rm () ())
rm args = MkCmd $ toCmd (path "rm") args []

rm' :: Expr Path -> Expr (Cmd Rm () ())
rm' p = rm defaults { rmPaths = [p] }

tee :: [Expr Path] -> Expr (Cmd Tee [StrSum] [StrSum])
tee ps = MkCmd $ toCmd (path "tee") defaults { teePaths = ps } []

cmd' :: Expr Path -> [Expr StrSum] -> [Redirect] -> Expr (Cmd AnyPath [StrSum] [StrSum])
cmd' p s = MkCmd . toCmd p (A s)
