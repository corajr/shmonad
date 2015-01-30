{-# LANGUAGE GADTs #-}
module Control.Monad.Shmonad.Conditional where

class Boolean b
class Command c

data If b where
  If :: (Boolean b) => b -> If b

data PartialCond b c where
  Then :: (Boolean b, Command c) => If b -> c -> PartialCond b c
  ElifThen :: (Boolean b, Command c) => PartialCond b c -> b -> c -> PartialCond b c

data Cond b c where
  ElseFi :: (Boolean b, Command c) => PartialCond b c -> c -> Cond b c
  Fi :: (Boolean b, Command c) => PartialCond b c -> Cond b c
