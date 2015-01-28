{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.ShmonadSpec (main, spec) where

import Test.Hspec
import Control.Monad.Shmonad
import Data.Monoid ((<>))
import System.Process
import System.Exit
import System.IO (hClose)
import System.IO.Temp
import qualified Data.Text.Lazy.IO as LIO

main :: IO ()
main = hspec spec

script1 :: Script ()
script1 = do
  hello <- newVar "hello" "Hello, "
  world <- newVar "world" "World!"
  setVar hello (Var hello <> Var world)
  echo ("hello: " <> Var hello)
  echo ("world: " <> Var world)
  x <- newVar "x" 2
  y <- newVar "y" 5
  exit (Var x + Var y)

script1result :: (ExitCode, String, String)
script1result 
  = ( ExitFailure 7
    , unlines [ "hello: Hello, World!"
              , "world: World!"
              ]
    , ""
    )

runScript :: Script a -> [String] -> String -> IO (ExitCode, String, String)
runScript script args' stdin =
  withSystemTempFile "script.sh" $ \path' h -> do
    LIO.hPutStr h $ toShellScript script
    hClose h
    readProcessWithExitCode "bash" (path':args') stdin

runScript' :: Script a -> IO (ExitCode, String, String)
runScript' script = runScript script [] ""

spec :: Spec
spec =
  describe "The toShellScript function" $ do
    it "should return the text of a trivial Script" $ do
      let script = toShellScript (exit 0) 
      script `shouldBe` "exit 0\n"
    it "should generate the text of a small Script and give expected output" $ do
      let script1' = runScript' script1
      script1' `shouldReturn` script1result
