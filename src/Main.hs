{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Shmonad
import Control.Monad
import qualified Data.Text.Lazy as L

default (L.Text)

script1 :: Script ()
script1 = forM_ [1..5] $ \i -> do
    x <- newVar "x" (Lit (i :: Integer))
    setVar x (Var x + 5)
    exec $ echo (Var x)

main :: IO ()
main = putStrLn $ L.unpack (toShellScript script1)
