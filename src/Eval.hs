{-# LANGUAGE LambdaCase #-}

module Eval where
  
  import LispType
  import Parser
  
  import Data.IORef
  import System.IO
  import Text.ParserCombinators.Parsec (parse)
  import Control.Concurrent
  import Data.Maybe
  
  import qualified Data.Map as Map
  import Control.Monad.Trans.Cont
  import Control.Monad.IO.Class
  import Control.Monad.State
  import Control.Monad
  import Data.List
  
  
  emptyState :: IO LispState
  emptyState = do
    ref <- newIORef =<< convertToRef initialEnvironment
    return $ LispState {
        environment = ref,
        stack = [],
        this = mempty
    }
    
  runInterpreter :: LispInterpreter () -> IO ()
  runInterpreter interpreter = do
    emptyState >>=
        runStateT (runContT interpreter $ const $ return $ Right nilValue) >>= \(a,_) ->
            handleRet a
        where
          handleRet (Left err) = hPutStrLn stderr $ "error: " ++ show err
          handleRet (Right _) = return ()
          
  evalProgram :: [LispData] -> IO ()
  evalProgram lsp =
    runInterpreter $
        forM_ lsp eval

  eval :: LispData -> LispInterpreter LispData
  eval (LispQuote q) = return q
  eval (LispSymbol sym) =
    lookupStack sym >>= \case
      Just a -> liftIO $ readIORef a
      Nothing -> lookupThis sym >>= \case
        Just a -> liftIO $ readIORef a
        Nothing -> lookupEnv sym >>= \case
            Nothing -> throwErr (ErrUndefined sym)
            Just var -> liftIO . readIORef $ var
        
  eval (LispCons ((LispSymbol "define"):(LispSymbol sym):var:[])) = do
    var' <- eval var
    defineVar sym var'
  eval (LispCons ((LispSymbol "define"):(LispCons ((LispSymbol sym):args)):body)) = do
    symbols <- getSymbols body
    defineVar sym $ LispLambda $ LambdaFn {
        closure = symbols,
        arguments = args,
        func = body
    }
  eval (LispCons ((LispSymbol "set!"):(LispSymbol sym):var:[])) = do
    lookupStack sym >>= (\case
        Just v -> do
            var' <- (liftIO . newIORef) =<< eval var
            modifyStack $ snd . mapAccumL (\stop frame -> do
                if stop then
                  (True,frame)
                else
                    case Map.lookup sym frame of
                        Nothing -> (True,frame)
                        Just _ -> (True,Map.insert sym var' frame)) False
            liftIO $ readIORef v
        Nothing -> do
            (Map.lookup sym <$> getThis) >>= \case
                Just ref -> do
                    var' <- eval var
                    oldRef <- liftIO $ readIORef ref
                    liftIO $ writeIORef ref var'
                    return oldRef
                Nothing ->
                    (Map.lookup sym <$> ((liftIO . readIORef) =<< getEnvironment)) >>= \case
                        Just _ -> eval var >>= defineVar sym
                        Nothing -> throwErr $ ErrUndefined sym)
        
  eval (LispCons ((LispSymbol "lambda"):(LispCons args):body)) = do
    capturedEnv <- getSymbols body
    
    return $ LispLambda $ LambdaFn {
        closure = capturedEnv,
        arguments = args,
        func = body
    }
    
  eval (LispCons [LispSymbol "quote", x]) = return x
  eval (LispCons ((LispSymbol "let"):(LispCons bindings):body)) = do
        bindings' <- Map.fromList <$> forM bindings (\(LispCons [LispSymbol nm, var]) ->
            eval var >>= (liftIO . newIORef) >>= \v -> return (nm,v))
        withNewStackFrame bindings' $
            foldM (const eval) nilValue body
        
  eval (LispCons ((LispSymbol "if"):cond:condTrue:condFalse:[])) = do
    isTrue <- eval cond
    case isTrue of
      LispBool False ->
        eval condFalse
      LispCons [] ->
        eval condFalse
      _ -> eval condTrue
    
  eval (LispCons (sym:args)) = do
    res <- eval sym
    evalArgs <- mapM eval args
    case res of
      (LispBuiltin fn) -> fn evalArgs
      (LispLambda fn) -> evalLambda fn evalArgs
      _ -> throwErr $ ErrNotLambda ""
  eval a = return a

  evalLambda :: LambdaFn -> [LispData] -> LispInterpreter LispData
  evalLambda fn args = do
    argBindings <- Map.fromList <$> zipWithM (\(LispSymbol a) b -> do
        b' <- liftIO $ newIORef b
        return (a,b')) (arguments fn) args
    withNewStack (closure fn) $ withNewStackFrame argBindings $ do
      foldM (const eval) nilValue (func fn)
  
  
  getSymbols :: [LispData] -> LispInterpreter Env
  getSymbols body = callCC $ \exit -> do
    getStack >>= \s -> when (null s) $ exit emptyEnv
    Map.fromList . concat <$> traverse iterateBody body
         where iterateBody = \case
                (LispSymbol n) ->
                   lookupStack n >>= \case
                        Nothing -> lookupThis n >>= \case
                            Nothing -> return []
                            Just a -> return [(n,a)]
                        Just a -> return [(n,a)]
                (LispCons lst) -> concat <$> traverse iterateBody lst
                (LispDotList lst dot) -> (++) <$> iterateBody dot <*> (concat <$> traverse iterateBody lst)
                _ -> return []

  defineVar :: String -> LispData -> LispInterpreter LispData
  defineVar sym var = do
        iovar <- liftIO $ newIORef var
        e <- getEnvironment
        liftIO $ modifyIORef' e $ Map.insert sym iovar
        return var
    
  initialEnvironment :: Map.Map String LispData
  initialEnvironment = Map.fromList [
      ("+",LispBuiltin plus),
      ("-",LispBuiltin minus),
      ("*",LispBuiltin mult),
      ("/",LispBuiltin divide),
      ("=",LispBuiltin eqFN),
      (">",LispBuiltin greaterFN),
      ("<",LispBuiltin lowerFN),
      ("not",LispBuiltin notFN),
      ("display",LispBuiltin printFN),
      ("displayln",LispBuiltin printlnFN),
      ("read",LispBuiltin getlineFN),
      ("eval",LispBuiltin evalFN),
      ("inspectStack",LispBuiltin printStackFrame),
      ("cons",LispBuiltin cons),
      ("car",LispBuiltin car),
      ("cdr",LispBuiltin cdr),
      ("procedure?",LispBuiltin procedure),
      ("number?",LispBuiltin number),
      ("boolean?",LispBuiltin isBool),
      ("list?",LispBuiltin isList),
      ("string?",LispBuiltin isString),
      ("call/cc",LispBuiltin callcc)
    ]
    
  convertToRef :: Map.Map String LispData -> IO Env
  convertToRef m =
    Map.fromList <$> forM (Map.toList m) (\(k,a) -> do
      ref <- newIORef a
      return (k,ref))
  
  plus :: BuiltinFn
  plus args =
    return . LispNumber . sum $ map (\(LispNumber n) -> n) args
    
  mult :: BuiltinFn
  mult args =
    return . LispNumber . product $ map (\(LispNumber n) -> n) args
    
  minus :: BuiltinFn
  minus ((LispNumber a):args) =
    return . LispNumber . foldl (-) a $ map (\(LispNumber n) -> n) args
    
  divide :: BuiltinFn
  divide ((LispNumber a):args) =
    return . LispNumber . foldl div a $ map (\(LispNumber n) -> n) args
    
  cons :: BuiltinFn
  cons (a:(LispCons rest):[]) =
    return (LispCons (a:rest))
    
  eqFN :: BuiltinFn
  eqFN ((LispNumber n):(LispNumber m):[]) =
    return (LispBool $ n == m)
    
  lowerFN :: BuiltinFn
  lowerFN ((LispNumber n):(LispNumber m):[]) =
    return (LispBool $ n < m)
    
  greaterFN :: BuiltinFn
  greaterFN ((LispNumber n):(LispNumber m):[]) =
    return (LispBool $ n > m)
    
  notFN :: BuiltinFn
  notFN ((LispBool n):[]) =
    return (LispBool $ not n)
    
  car :: BuiltinFn
  car ((LispCons (c:rest)):[]) =
    return c
    
  cdr :: BuiltinFn
  cdr ((LispCons (c:rest)):[]) =
    return (LispCons rest)
    
  procedure :: BuiltinFn
  procedure ((LispBuiltin _):[]) =
    return (LispBool True)
  procedure ((LispLambda _):[]) =
    return (LispBool True)
  procedure _ =
    return (LispBool False)
    
  number :: BuiltinFn
  number ((LispNumber _):[]) =
    return (LispBool True)
  number _ =
    return (LispBool False)
    
  isBool :: BuiltinFn
  isBool ((LispBool _):[]) =
    return (LispBool True)
  isBool _ =
    return (LispBool False)
    
  isList :: BuiltinFn
  isList ((LispCons _):[]) =
    return (LispBool True)
  isList ((LispDotList _ _):[]) =
    return (LispBool True)
  isList _ =
    return (LispBool False)
    
  isString :: BuiltinFn
  isString ((LispString _):[]) =
    return (LispBool True)
  isString _ =
    return (LispBool False)
    
  evalFN :: BuiltinFn
  evalFN ((LispString str):_) = eval $ either  (const nilValue) id $ parse parseLisp "<stdin>" str
  
  printFN :: BuiltinFn
  printFN args = do
    forM_ args (\a -> liftIO $ putStr (show a) >> hFlush stdout)
    return nilValue
    
  printlnFN :: BuiltinFn
  printlnFN args = do
    forM_ args (\a -> liftIO $ print a >> hFlush stdout)
    return nilValue
    
  getlineFN :: BuiltinFn
  getlineFN _ = do
    line <- liftIO $! getLine
    return (LispString line)
    
  printStackFrame :: BuiltinFn
  printStackFrame _ = do
    currStack <- getStack
    liftIO $ print "inspecting..."
    forM_ currStack $ \e -> do
      liftIO $ putStrLn "\tframe --------"
      forM_ (Map.toList e) $ \(k,aref) -> do
        a <- liftIO $ readIORef aref
        liftIO . putStrLn $ "\t\tkey: " ++ show k ++ ", value: " ++ show a
    return nilValue
    
  callcc :: BuiltinFn
  callcc [LispLambda lam] = callCC $ \k -> do
    saveStack <- getStack
    let cc :: BuiltinFn
        cc [ret] = do
            modifyStack $ const saveStack
            k ret
    evalLambda lam [LispBuiltin cc]
    
    
    
    
