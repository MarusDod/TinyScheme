module Main where

    import Lib

    import Control.Monad.Trans.Cont
    import Control.Monad.State
    import System.IO
    import System.Environment
    import System.Console.GetOpt

    import LispType
    import Parser
    import Text.ParserCombinators.Parsec (parse)
    import Eval
    import Data.Maybe (fromMaybe)
    import Control.Monad.Except

    compileFile :: String -> IO ()
    compileFile filepath = do
      contents <- readFile filepath
      case parseTree filepath contents of
        Left err -> print err
        Right ast -> do
            print ast
            evalProgram ast
            
    main :: IO ()
    main = do
      args <- getArgs
      case getOpt Permute optionsDesc args of
        (op,_,[]) -> do
          let opt = foldl (flip ($)) defaultOptions op
          if optShowVersion opt then
            print "TinyScheme 1.0.0"
          else maybe (compileFile "lisp/repl.scm") compileFile (optFilePath opt)
        (_,_,errs) -> ioError $ userError $ usageInfo "Usage:" optionsDesc

    data Options = Options {
        optShowVersion :: Bool,
        optHelp :: Bool,
        optFilePath :: Maybe String
    } deriving Show
    
    defaultOptions :: Options
    defaultOptions = Options {
        optFilePath = Nothing,
        optHelp = False,
        optShowVersion = False
    }
    
    optionsDesc :: [OptDescr (Options -> Options)]
    optionsDesc = [
            Option ['v'] ["version"]  (NoArg $ \opt -> opt {optShowVersion = True}) "show version",
            --Option ['h'] ["help"]  (NoArg $ \opt -> opt {optVerbose = True}),
            Option ['f'] ["file"]  (OptArg (\f opt -> opt {optFilePath = f}) "FILE") "interpret FILE"
        ]

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
