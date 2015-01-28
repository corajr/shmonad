{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding ((++))
import Control.Monad.Shmonad
import Control.Monad
import qualified Data.Text.Lazy as L

default (L.Text)

script1 :: Script ()
script1 = do
    hello <- newVar "hello" "Hello, "
    world <- newVar "world" "World!"
    setVar hello (Var hello ++ Var world)
    echo ("hello: " ++ Var hello)
    echo ("world: " ++ Var world)
    x <- newVar "x" 2
    y <- newVar "y" 5
    exit (Var x + Var y)

script2 :: Script ()
script2 = forM_ [1..5] $ \i -> do
    x <- newVar "x" (Lit (i :: Integer))
    setVar x (Var x + 5)
    echo (Shown (Var x))

main :: IO ()
main = putStrLn $ L.unpack (toShellScript script1)
