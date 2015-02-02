{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Shmonad.StatementSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text.Lazy as L
import Data.Number.Nat
import Control.Applicative
import Control.Monad.Free
import Control.Monad.RWS.Lazy
import Control.Monad.Shmonad.Command
import Control.Monad.Shmonad.Expression
import Control.Monad.Shmonad.Statement

import Control.Monad.Shmonad.ExpressionSpec(VStr(..), VInt(..))

default (L.Text)

instance (Arbitrary a, Variable a) => Arbitrary (Statement (Free Statement a)) where
  arbitrary = do
    VStr str <- arbitrary
    RunCommand <$> echo <$> return str <*> arbitrary

instance (Arbitrary a, Variable a) => Arbitrary (Script a) where
  arbitrary = Free <$> arbitrary

{- oneof [newVint, newVstr, echoS, exitE]
      where newVstr = do
            VStr val <- arbitrary
            return $ NewVar <$> arbitrary <*> val <*> ()
          newVint = do
            VStr val <- arbitrary
            NewVar <$> arbitrary <*> val <*> ()
          echoS = Echo <$> arbitrary <*> ()
          exitE = Exit <$> arbitrary <*> ()
-}

prop_transpile :: Script a -> Bool
prop_transpile a = let (_, _, w) = runRWS (transpile a) () (0 :: Nat)
                   in not $ L.null w

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "A Statement" $ do
    it "can be transpiled (example)" $ do
      let state' = Free (RunCommand (exit 0) (Pure ()))
      let (_, _, w) = runRWS (transpile state') () (0 :: Nat)
      w `shouldBe` "exit 0\n"
    it "can be transpiled" $ property $ 
      \(Blind (x :: Script Integer)) -> prop_transpile x
