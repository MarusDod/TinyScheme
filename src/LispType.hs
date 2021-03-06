{-# LANGUAGE LambdaCase #-}

module LispType where
  
  import Data.IORef
  import qualified Data.Map as Map
  import Control.Monad.State
  import Control.Monad.Trans.Cont
  
  
  type Env = Map.Map String (IORef LispData)
  type EnvRef = IORef Env
  
  type Stack = [Env]
  type This = Env
  
  type LispThrowErr = Either LispErr LispData
  
  data LispState = LispState {
    environment :: EnvRef,
    stack :: Stack,
    this :: This
  }
  
  type LispInterpreter a = ContT LispThrowErr (StateT LispState IO) a
  
  type BuiltinFn = [LispData] -> LispInterpreter LispData
  
  data LambdaFn = LambdaFn {
    closure :: Env,
    arguments :: [LispData],
    func :: [LispData]
  }
  
  data LispData
    = LispNumber Integer
    | LispSymbol String
    | LispBool Bool
    | LispString String
    | LispQuote LispData
    | LispCons [LispData]
    | LispDotList [LispData] LispData
    | LispBuiltin BuiltinFn
    | LispLambda LambdaFn
    
  data LispType
    = NUMBER
    | STRING
    | SYMBOL
    | BOOL
    | FUNCTION
    | LIST
    deriving Show

  data LispErr
    = ErrUnreachable
    | ErrArgumentsMismatch Integer Integer
    | ErrNotLambda String
    | ErrUndefined String
    | ErrTypeMismatch LispType LispType
    deriving Show

  instance Show LispData where
    show (LispNumber n) = show n
    show (LispString n) = show n
    show (LispSymbol n) = n
    show (LispBool n) = if n then "#t" else "#f"
    show (LispQuote n) = "'" ++ show n
    show (LispCons n) = "(" ++ unwords (map show n) ++ ")"
    show (LispDotList n dot) = "(" ++ unwords (map show n) ++ " . " ++ show dot ++ ")"
    show (LispBuiltin _) = "<builtin-primitive>"
    show (LispLambda _) = "<lambda>"
    
  clone :: IORef LispData ->  IO (IORef LispData)
  clone ref =
    readIORef ref >>= \case
        LispCons _ -> return ref
        LispDotList _ _ -> return ref
        a -> newIORef a

  throwErr :: LispErr -> LispInterpreter a
  throwErr err = ContT $ \_ -> return $ Left err
  
  newInterpreter :: LispInterpreter ()
  newInterpreter = return ()
  
  emptyEnv = Map.empty
  
  nilValue = LispCons []
  
  liftState = lift
  
  getStack :: LispInterpreter Stack
  getStack = stack <$> liftState get
  
  getThis :: LispInterpreter This
  getThis = this <$> liftState get
  
  lookupThis :: String -> LispInterpreter (Maybe (IORef LispData))
  lookupThis nm =
    Map.lookup nm <$> getThis
  
  setThis :: This -> LispInterpreter ()
  setThis newThis = do
    liftState $ modify $ \s -> s {this = newThis}
    
  changeThis :: String -> LispData -> LispInterpreter LispData
  changeThis nm lsp =
    lookupThis nm >>= \case
        Nothing -> throwErr $ ErrUndefined $ "wtf no closure variable set with name: " <> nm
        Just ref -> liftIO (readIORef ref) >>= \x ->
            liftIO (writeIORef ref lsp) >>
            return x
  
  getEnvironment :: LispInterpreter EnvRef
  getEnvironment = environment <$> liftState get
  
  modifyStack :: (Stack -> Stack) -> LispInterpreter ()
  modifyStack fn =
    liftState $ modify $ \s -> s {stack = fn $ stack s}
    
  lookupStack :: String -> LispInterpreter (Maybe (IORef LispData))
  lookupStack nm = do
    getStack >>= \st ->
        foldM iterateStack Nothing st
            where iterateStack :: Maybe (IORef LispData) -> Env -> LispInterpreter (Maybe (IORef LispData))
                  iterateStack mres a = 
                    case mres of
                       Just res -> return $ Just res
                       Nothing -> return $ Map.lookup nm a
                       
  lookupEnv :: String -> LispInterpreter (Maybe (IORef LispData))
  lookupEnv sym = do
       env <- (liftIO . readIORef) =<< getEnvironment
       return $ Map.lookup sym env
    
  pushStackFrame :: LispInterpreter ()
  pushStackFrame = do
    modifyStack ([emptyEnv] ++)
    
  popStackFrame :: LispInterpreter ()
  popStackFrame = do
    modifyStack tail
    
  addToStackFrame :: String -> (IORef LispData) -> LispInterpreter ()
  addToStackFrame nm var = do
    modifyStack $ \s -> Map.insert nm var (head s) : tail s
    
  withNewStackFrame :: Env -> LispInterpreter a -> LispInterpreter a
  withNewStackFrame newVars fn = do
    pushStackFrame
    forM_ (Map.toList newVars) $ uncurry addToStackFrame
    x <- fn
    popStackFrame
    return x
    
  withNewStack :: Env -> LispInterpreter a -> LispInterpreter a
  withNewStack clos fn = do
    oldThis <- getThis
    oldStack <- getStack
    setThis clos
    modifyStack $ const []
    ret <- fn
    setThis oldThis
    modifyStack $ const oldStack
    return ret
    
    