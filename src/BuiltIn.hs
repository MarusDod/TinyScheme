module BuiltIn where
  
  import LispType
  import Control.Monad.Cont (liftIO)
  import Control.Monad
  import qualified Data.Map as Map
  import Data.IORef
  
  initialEnvironment :: Map.Map String LispData
  initialEnvironment = Map.fromList [
      ("+",LispBuiltin plus),
      ("-",LispBuiltin minus),
      ("*",LispBuiltin mult),
      ("/",LispBuiltin divide),
      ("print",LispBuiltin printFN),
      ("cons",LispBuiltin cons)
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

  printFN :: BuiltinFn
  printFN args = do
    forM_ args (liftIO . print)
    return nilValue
    
    
    