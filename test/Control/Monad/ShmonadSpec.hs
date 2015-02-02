{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.ShmonadSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((++))
import Control.Monad.Shmonad
import Data.Monoid ((<>))
import System.Process
import System.Exit
import System.IO (hClose)
import System.IO.Temp
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

main :: IO ()
main = hspec spec

script1 :: Script ()
script1 = do
  hello <- newVar "hello" (quote "Hello, ")
  world <- newVar "world" (quote "World!")
  setVar hello (Var hello <> Var world)
  exec $ echo ("hello: " <> Var hello)
  exec $ echo ("world: " <> Var world)
  x <- newVar "x" 2
  y <- newVar "y" 5
  exec $ exit (Var x + Var y)

script1result :: (ExitCode, String, String)
script1result 
  = ( ExitFailure 7
    , unlines [ "hello: Hello, World!"
              , "world: World!"
              ]
    , ""
    )

scriptLs :: Script ()
scriptLs = do
  exec ls
  exec $ exit 0

{-
scriptWithConditionals :: Script ()
scriptWithConditionals = do
  p <- varFromEnv "PATH"
  if_ p .==. "/notlikely"
     (then_ $ echo p <> " is an unlikely PATH value")
     (else_ $ echo p <> " is reasonable")
-}

runScriptText :: L.Text -> [String] -> String -> IO (ExitCode, String, String)
runScriptText text args' stdin =
  withSystemTempFile "script.sh" $ \path' h -> do
    LIO.hPutStr h text
    hClose h
    readProcessWithExitCode "bash" (path':args') stdin

runScript :: Script a -> [String] -> String -> IO (ExitCode, String, String)
runScript script = runScriptText (toShellScript script)

runScript' :: Script a -> IO (ExitCode, String, String)
runScript' script = runScript script [] ""

checkComp :: (Integer -> Integer -> Bool) -> (Expr Integer -> Expr Integer -> Expr ShBool) -> (Integer, Integer) -> Property
checkComp c c' (a, b) = monadicIO test'
  where comp = a `c` b
        test' = do let e = Lit a `c'` Lit b
                   let e' = "if " <> shExpr e <> "; then exit 0; else exit 1; fi"
                   (r, _, _) <- run $ runScriptText e' [] ""
                   assert $ if comp then r == ExitSuccess else r > ExitSuccess

spec :: Spec
spec = do
  describe "The toShellScript function" $ do
    it "should return the text of a trivial Script" $ do
      let script = toShellScript (exec $ exit 0) 
      script `shouldBe` "exit 0\n"
    it "should generate the text of a small Script and give expected output" $ do
      let script1' = runScript' script1
      script1' `shouldReturn` script1result
  describe "Number comparisons" $ do
    it "should correctly answer (==)" $ property $ checkComp (==) (.==.)
    it "should correctly answer (/=)" $ property $ checkComp (/=) (./=.)
    it "should correctly answer (<)" $ property $ checkComp (<) (.<.)
    it "should correctly answer (<=)" $ property $ checkComp (<=) (.<=.)
    it "should correctly answer (>)" $ property $ checkComp (>) (.>.)
    it "should correctly answer (>=)" $ property $ checkComp (>=) (.>=.)
  describe "Running a command" $ do
    it "should run a basic command" $ do
      (r, s, _) <- runScript' scriptLs
      r `shouldBe` ExitSuccess
      s `shouldSatisfy` not . null
    it "should run with specified args" $ do
      pending
