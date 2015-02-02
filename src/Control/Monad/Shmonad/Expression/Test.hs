{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.Expression.Test where

import Control.Monad.Shmonad.Expression.Types

class ShTest a where
  toTestStr :: a -> Str

data StrCheck = NonZero | ZeroLength
  deriving Show

instance ShTest StrCheck where
  toTestStr x
    = case x of
        NonZero -> "-n"
        ZeroLength -> "-z"

-- | Enumeration of numerical comparisons (==, <, >, ...).
data CompareOp = Equal | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual
  deriving Show

instance ShTest CompareOp where
  toTestStr x
    = case x of
        Equal -> "-eq"
        GreaterThan -> "-gt"
        GreaterOrEqual -> "-ge"
        LessThan -> "-lt"
        LessOrEqual -> "-le"

data PathCheck
  = BlockSpecial
  | CharSpecial
  | Directory
  | Existing
  | RegularFile
  | SetGIDFile
  | SymLink
  | IsFIFO
  | Readable
  | Socket
  | NonZeroSize
  | SetUIDFile
  | Writable
  | Executable
  deriving Show

instance ShTest PathCheck where
  toTestStr x
    = case x of
        BlockSpecial -> "-b"
        CharSpecial -> "-c"
        Directory -> "-d"
        Existing -> "-e"
        RegularFile -> "-f"
        SetGIDFile -> "-g"
        SymLink -> "-L" -- -h is synonym
        IsFIFO -> "-p"
        Readable -> "-r"
        Socket -> "-S"
        NonZeroSize -> "-s"
        SetUIDFile -> "-u"
        Writable -> "-w"
        Executable -> "-x"

