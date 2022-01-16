module Main where

import Lib

import Control.Monad.Trans.Cont
import Control.Monad.State
import System.IO
import System.Environment

import LispType
import Parser
import Text.ParserCombinators.Parsec (parse)


main :: IO ()
main = do
  --print $ parse parseLisp "scm" "(lambda () (+ x 2))"
  (filepath:_) <- getArgs
  contents <- readFile filepath
  print $ parse parseLisp filepath contents
  


--foo :: ContT () (State [Int]) ()
--foo = do
--  lift $ modify $ \s -> s ++ [1]
--  resetT $ do
--    a <- shiftT $ \k -> do
--        lift $ forM_ [4..10] k
--    lift $ modify (++ [a])
--
--bar = print $ execState (runContT foo (const $ return ())) []
