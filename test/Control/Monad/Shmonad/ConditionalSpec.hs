{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Monad.Shmonad.ConditionalSpec (main, spec) where

import Test.Hspec
import Control.Monad.Shmonad.Conditional

data S b c where
  A :: S b c
  B :: S b c
  C :: S b c
  Conditional :: (Show b, Show c, Eq b, Eq c) => Cond b c -> S b c

deriving instance (Eq b) => Eq (If b)
deriving instance (Eq b, Eq c) => Eq (PartialCond b c)
deriving instance (Eq b, Eq c) => Eq (Cond b c)
deriving instance Eq (S b c)

instance Show (S b c) where
  show A = "A"
  show B = "B"
  show C = "C"
  show (Conditional c) = show c

instance Boolean Bool
instance CondCommand (S b c)

instance Show b => Show (If b) where
  show (If x) = "If " ++ show x

instance (Show b, Show c) => Show (PartialCond b c) where
  show (Then if' cmd) = show if' ++ " Then " ++ show cmd
  show (ElifThen pc b cmd) = show pc ++ " Elif " ++ show b ++ " Then " ++ show cmd

instance (Show b, Show c) => Show (Cond b c) where
  show (ElseFi pc cmd) = "(" ++ show pc ++ " Else " ++ show cmd ++ " Fi)"
  show (Fi pc) = "(" ++ show pc ++ " Fi)"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "An If" $
    it "takes a boolean value" $ do
      let x = If True
      show x `shouldBe` "If True"
  describe "A Then" $
    it "takes an If and a command" $ do
      let t = Then (If True) A
      show t `shouldBe` "If True Then A"
  describe "An ElifThen" $
    it "takes a partial condition and returns another partial condition" $ do
      let et = ElifThen (Then (If True) A) False B
      show et `shouldBe` "If True Then A Elif False Then B"
  describe "An ElseFi" $ do
    it "takes a simple partial condition and a command" $ do
      let ef = ElseFi (Then (If True) A) B
      show ef `shouldBe` "(If True Then A Else B Fi)"
    it "takes a complex partial condition and a command" $ do
      let ef = ElseFi (Then (If False) (Conditional (ElseFi (Then (If True) A) B))) C
      show ef `shouldBe` "(If False Then (If True Then A Else B Fi) Else C Fi)"
  describe "An Fi" $
    it "takes a partial condition and completes it" $ do
      let f = Fi (Then (If True) B)
      show f `shouldBe` "(If True Then B Fi)"
  describe "Smart constructors" $ do
    it "allows for if-then-fi" $ do
      let c = ifThen True A fi
      c `shouldBe` Fi (Then (If True) A)
    it "allows for if-then-else-fi" $ do
      let c = ifThen True A $
                elseFi B 
      c `shouldBe` ElseFi (Then (If True) A) B
    it "allows for if-then-elif-then-fi" $ do
      let c = ifThen True A $ 
                elifThen False B
                fi
      c `shouldBe` Fi (ElifThen (Then (If True) A) False B)
    it "allows for if-then-elif-then-elif-then-else-fi" $ do
      let c = ifThen True A $
                elifThen False B $
                elifThen True C $
                elseFi A
      c `shouldBe` ElseFi (ElifThen (ElifThen (Then (If True) A) False B) True C) A
