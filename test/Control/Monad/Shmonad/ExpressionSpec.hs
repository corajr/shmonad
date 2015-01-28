{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.ExpressionSpec (main, spec) where

import Test.Hspec
import Control.Monad.Shmonad.Expression

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  variableSpec
  expressionSpec

variableSpec :: Spec
variableSpec = do
  let var :: (VarID Integer)
      var = VarID 0 "hello"
  describe "A VarID" $ do
    it "has a unique numerical ID and a name" $ do
      varID var `shouldBe` 0
      varName var `shouldBe` "hello"
    it "has an associated type" $ do
      let var2 :: (VarID Str)
          var2 = VarID 1 "world"
      varID var2 `shouldBe` 1
      -- because of phantom type var, impossible to write:
      -- var2 `shouldSatisfy` (/=) var
  describe "The Variable typeclass" $
    it "should turn a VarID into a unique name" $
      uniqueName var `shouldBe` "hello0"

expressionSpec :: Spec
expressionSpec =
  describe "An Expr" $ do
    describe "Lit a" $
      it "should have a single value" $ do
        let l :: Expr Integer
            l  = Lit 1
        shExpr l `shouldBe` "1"
    describe "Var a" $
      it "turns a VarID into an expression" $ do
        let var :: Expr Str
            var = Var (VarID 2 "foo")
        shExpr var `shouldBe` "${foo2}"
    describe "Plus" $
      it "adds two Expr Integers" $ do
        let int1 = Lit 1
        let int2 = Lit 2
        let sumInts = Plus int1 int2
        shExpr sumInts `shouldBe` "$((1+2))"
    describe "Concat" $
      it "concatenates two Expr Strs" $ do
        let str1 = "Hello, "
        let str2 = "world!"
        let str = Concat str1 str2
        shExpr str `shouldBe` "\"Hello, \"\"world!\""

