{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.ShmonadSpec (main, spec) where

import Prelude hiding ((++))
import Test.Hspec
import Control.Monad.Shmonad

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "The toShellScript function" $
    it "should return the text of a Script" $ do
      let script = toShellScript (exit 0) 
      script `shouldBe` "exit 0\n"
