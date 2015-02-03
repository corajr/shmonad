{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.CommandSpec (main, spec) where

import Test.Hspec
import GHC.TypeLits
import Control.Monad.Shmonad.Expression
import Control.Monad.Shmonad.Command

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Cmd a" $ do
    it "should have a command path, list of args, and list of redirects" $ do
      let (ls' :: Cmd Ls () [StrSum]) = toCmd (path "ls") defaults []
      let p = shExpr (cmdName ls')
      p `shouldBe` "\"ls\""
    it "can be combined with And" $ do
      let a = ls `andThen` cd (varFromEnvUnsafe "HOME")
      shExpr a `shouldBe` "\"ls\" && cd ${HOME}"
    it "can be combined with Or" $ do
      let o = ls `orElse` cd (varFromEnvUnsafe "HOME")
      shExpr o `shouldBe` "\"ls\" || cd ${HOME}"
    it "can be combined with Pipe" $ do
      let p = ls `pipe` tee [path "/tmp/log.txt"]
      shExpr p `shouldBe` "\"ls\" | \"tee\" \"/tmp/log.txt\""
  describe "A Command" $ do
    it "defines an Args type" $ do
      let args = defaults :: Args Ls () [StrSum]
      symbolVal (flagSymbol (lsShowAll args)) `shouldBe` "-A"
      symbolVal (flagSymbol (lsLong args)) `shouldBe` "-l"
    it "stores default arguments" $ do
      let args = defaults :: Args Ls () [StrSum]
      flagBool (lsShowAll args) `shouldBe` False
      flagBool (lsLong args) `shouldBe` False
    it "turns Args into a list of Str expressions" $ do
      let argStr = argsToStr (defaults :: Args Ls () [StrSum])
      null argStr `shouldBe` True
    it "turns a path, args, and list of redirects into a Cmd a" $ do
      let c = cmd' (path "ls") ["-1"] [toFile (path "/tmp/log.txt")]
      shExpr c `shouldBe` "\"ls\" -1 > \"/tmp/log.txt\""
