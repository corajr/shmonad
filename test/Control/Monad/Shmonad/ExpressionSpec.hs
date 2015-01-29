{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Control.Monad.Shmonad.ExpressionSpec (main, spec, VStr(..), VInt(..)) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Control.Monad.Shmonad.Expression
import Control.Applicative ((<$>), (<*>))
import Data.Char (isDigit, chr)
import Data.Monoid ((<>))
import Data.Number.Nat
import qualified Data.Text.Lazy as L

default (L.Text)

randText :: Gen L.Text
randText = sized $ \n ->
  do k <- choose (0, n)
     L.pack <$> vectorOf k validChars
     where validChars = chr <$> choose (32, 126)

instance Arbitrary L.Text where
  arbitrary = randText

instance Arbitrary Name where
  arbitrary = toName <$> randText `suchThat` isValidName

instance Arbitrary Nat where
  arbitrary = do
    NonNegative x <- arbitrary :: Gen (NonNegative Integer)
    return $ toNat x

instance Variable a => Arbitrary (VarID a) where
  arbitrary = VarID <$> arbitrary <*> arbitrary

newtype VStr = VStr (Expr Str)
  deriving Show

instance Arbitrary VStr where
  arbitrary = VStr <$> Var <$> arbitrary

newtype VInt = VInt (Expr Integer)
  deriving Show

instance Arbitrary VInt where
  arbitrary = VInt <$> Var <$> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  variableSpec
  expressionSpec

variableSpec :: Spec
variableSpec = do
  describe "A Name" $ do
    it "must be non-empty" $ property $
      not . L.null . fromName
    it "must not start with a digit" $ property $
      \str -> not (L.null str) && isDigit (L.head str) ==>
        not (isValidName str)
    it "can only contain letters, digits and underscores" $ property $
      isValidName . fromName
  let var :: (VarID Integer)
      var = VarID 0 "hello"
  describe "A VarID" $ do
    it "has a unique numerical ID and a name" $ do
      varID var `shouldBe` 0
      let n = toName "hello"
      varName var `shouldBe` n
    it "has an associated type" $ do
      let var2 :: (VarID Str)
          var2 = VarID 1 "world"
      varID var2 `shouldBe` 1
      -- because of phantom type var, impossible to write:
      -- var2 `shouldSatisfy` (/=) var
  describe "The Variable typeclass" $ do
    it "should turn a VarID into a unique name" $ do
      let n = toName "hello0"
      uniqueName var `shouldBe` n
    it "should produce a unique name that is a valid shell name" $ property $
      \(VStr (Var x)) -> isValidName . fromName $ uniqueName x

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
    describe "Shown" $
      it "turns an Expr a with Show a into Expr Str" $ do
        let l :: Expr Integer
            l = Lit 1
        let s = Shown l
        shExpr s `shouldBe` "1"
    describe "StrEquals" $ do
      it "checks whether two strings are equal" $ do
        let l1 = Lit ("" :: L.Text)
        let l2 = Lit ("hi" :: L.Text)
        let eqs = l1 .==. l2
        shExpr eqs `shouldBe` "[ \"\" = \"hi\" ]"
      it "checks whether two strings are not equal" $ do
        let l1 = Lit ("" :: L.Text)
        let l2 = Lit ("hi" :: L.Text)
        let ne = l1 ./=. l2
        shExpr ne `shouldBe` "! [ \"\" = \"hi\" ]"
    describe "NumCompare" $ do
      let n1 = Lit (0 :: Integer)
      let n2 = Lit 1
      it "checks whether two numbers are equal" $ do
        let eq = n1 .==. n2
        shExpr eq `shouldBe` "[ 0 -eq 1 ]"
      it "checks whether two numbers are not equal" $ do
        let eq = n1 ./=. n2
        shExpr eq `shouldBe` "! [ 0 -eq 1 ]"
      it "checks whether one numbers is less than another" $ do
        let lt = n1 .<. n2
        shExpr lt `shouldBe` "[ 0 -lt 1 ]"
      it "checks whether one number is less than or equal to another" $ do
        let le = n1 .<=. n2
        shExpr le `shouldBe` "[ 0 -le 1 ]"
      it "checks whether one number is greater than another" $ do
        let gt = n1 .>. n2
        shExpr gt `shouldBe` "[ 0 -gt 1 ]"
      it "checks whether one number is greater than or equal to another" $ do
        let ge = n1 .>=. n2
        shExpr ge `shouldBe` "[ 0 -ge 1 ]"

