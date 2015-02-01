{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.CommandSpec (main, spec) where

import Test.Hspec

import Control.Monad.Shmonad.Expression
import Control.Monad.Shmonad.Command

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Cmd a" $ do
    it "should have a command path, list of args, and list of redirects" $ do
      let (ls' :: Cmd Ls) = toCmd (path "ls") defaults []
      let p = shExpr (cmdName ls')
      p `shouldBe` "\"ls\""
    it "can be combined with And" $ do
      let a = ls `andThen` cd (varFromEnvUnsafe "HOME")
      shExpr a `shouldBe` "\"ls\" && cd ${HOME}"
    it "can be combined with Or" $ do
      let o = ls `orElse` cd (varFromEnvUnsafe "HOME")
      shExpr o `shouldBe` "\"ls\" || cd ${HOME}"
    it "can be combined with Pipe" $ do
      pending
  describe "A Command" $ do
    it "defines an Args type" $ do
      pending
    it "stores default arguments" $ do
      pending
    it "turns Args into a list of Str expressions" $ do
      pending
    it "turns a path, args, and list of redirects into an Expr (Cmd a)" $ do
      pending
