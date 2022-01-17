module Main where

import Lib

import Control.Monad.Trans.Cont
import Control.Monad.State
import System.IO
import System.Environment

import LispType
import Parser
import Text.ParserCombinators.Parsec (parse)
import Eval
import BuiltIn


main :: IO ()
main = do
  print $ parse parseLisp "scm" "(lambda () (+ x 2))"
  (filepath:_) <- getArgs
  contents <- readFile filepath
  case parseTree filepath contents of
    Left err -> print err
    Right ast -> do
        print ast
        evalProgram ast


--foo :: ContT () (State [Int]) ()
--foo = do
--  lift $ modify $ \s -> s ++ [1]
--  resetT $ do
--    a <- shiftT $ \k -> do
--        lift $ forM_ [4..10] k
--    lift $ modify (++ [a])
--    ContT $ const $ return ()
--
--bar = print $ execState (runContT foo (const $ return ())) []
