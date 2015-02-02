{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Shmonad.Expression.Test where

import Control.Monad.Shmonad.Expression.Types

class ShTest a where
  data Test a :: *
  toTestStr :: Test a -> Str

instance ShTest StrSum where
  data Test StrSum = NonZero | ZeroLength
    deriving Show
  toTestStr x
    = case x of
        NonZero -> "-n"
        ZeroLength -> "-z"

instance ShTest Integer where
  data Test Integer = Equal | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual
    deriving Show
  toTestStr x
    = case x of
        Equal -> "-eq"
        GreaterThan -> "-gt"
        GreaterOrEqual -> "-ge"
        LessThan -> "-lt"
        LessOrEqual -> "-le"

instance ShTest Path where
  data Test Path = BlockSpecial
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

