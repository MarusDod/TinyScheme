{-# LANGUAGE LambdaCase #-}

module Eval where
  
  import LispType
  
  import Data.IORef
  import Data.Maybe
  import qualified Data.Map as Map
  import Control.Monad.Trans.Cont
  import Control.Monad.IO.Class
  import Control.Monad.State
  import Control.Monad
  
  
  emptyState :: IO LispState
  emptyState = do
    ref <- newIORef =<< convertToRef initialEnvironment
    return $ LispState {
        environment = ref,
        stack = []
    }
    
  runInterpreter :: LispInterpreter () -> IO ()
  runInterpreter interpreter =
    emptyState >>=
        runStateT (runContT interpreter (const $ return $ Right nilValue)) >>
            return ()
            
  evalProgram :: [LispData] -> IO ()
  evalProgram lsp =
    runInterpreter $
        forM_ lsp eval

  eval :: LispData -> LispInterpreter LispData
  eval (LispQuote q) = return q
  eval (LispSymbol sym) = callCC $ \exit -> do
    frameMaybe <- listToMaybe <$> getStack
    case frameMaybe of
      Nothing -> lookupEnv >>= \case
        Nothing -> throwErr (ErrUndefined sym)
        Just var -> liftIO . readIORef $ var
      Just frame -> case Map.lookup sym frame of
        Just var -> liftIO . readIORef $ var
        Nothing -> lookupEnv >>= \case
            Nothing -> throwErr (ErrUndefined sym)
            Just var -> liftIO . readIORef $ var
        
    where lookupEnv = do
            env <- (liftIO . readIORef) =<< getEnvironment
            return $ Map.lookup sym env
            
  eval (LispCons ((LispSymbol "define"):(LispSymbol sym):var:[])) = do
    var' <- eval var
    iovar <- liftIO $ newIORef var'
    e <- getEnvironment
    liftIO $ modifyIORef' e $ Map.insert sym iovar
    return var'
  eval (LispCons ((LispSymbol "lambda"):(LispCons args):body)) = do
    capturedEnv <- getSymbols body
    
    return $ LispLambda $ LambdaFn {
        closure = capturedEnv,
        arguments = args,
        func = body
    }
    
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
    withNewStackFrame (argBindings `Map.union` closure fn) $ do
      foldM (const eval) nilValue (func fn)
  
  
  getSymbols :: [LispData] -> LispInterpreter Env
  getSymbols body = callCC $ \exit -> do
    stackFrameMaybe <- listToMaybe <$> getStack
    when (isNothing stackFrameMaybe) $ exit emptyEnv
    
    let stackFrame = fromJust stackFrameMaybe
    
    let iterateBody = \case
                (LispSymbol n) ->
                   case Map.lookup n stackFrame of
                        Nothing -> return []
                        Just _ -> return [n]
                (LispCons lst) -> concat <$> traverse iterateBody lst
                (LispDotList lst dot) -> (++) <$> iterateBody dot <*> (concat <$> traverse iterateBody lst)
                _ -> return []
    symbols <- concat <$> traverse iterateBody body
    return $ Map.fromList $ catMaybes $ flip map symbols $ \s -> do
      case Map.lookup s stackFrame of
        Nothing -> Nothing
        Just var -> Just (s,var)
    
    
    
  initialEnvironment :: Map.Map String LispData
  initialEnvironment = Map.fromList [
      ("+",LispBuiltin plus),
      ("-",LispBuiltin minus),
      ("*",LispBuiltin mult),
      ("/",LispBuiltin divide),
      ("display",LispBuiltin printFN),
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

  printFN :: BuiltinFn
  printFN args = do
    forM_ args (liftIO . print)
    return nilValue
    
  callcc :: BuiltinFn
  callcc [LispLambda lam] = callCC $ \k -> do
    let cc :: BuiltinFn
        cc [ret] = k ret
    evalLambda lam [LispBuiltin cc]
    
    
    
    
