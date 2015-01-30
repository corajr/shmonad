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

ifThen :: (Boolean b, Command c) => b -> c -> (PartialCond b c -> Cond b c) -> Cond b c
ifThen b c f = f (Then (If b) c)

elifThen :: (Boolean b, Command c) => b -> c -> (PartialCond b c -> Cond b c) -> PartialCond b c -> Cond b c
elifThen b c f i = f (ElifThen i b c)

elseFi :: (Boolean b, Command c) => c -> PartialCond b c -> Cond b c
elseFi = flip ElseFi

fi :: (Boolean b, Command c) => PartialCond b c -> Cond b c
fi = Fi
