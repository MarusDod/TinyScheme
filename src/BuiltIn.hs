module BuiltIn where
  
  import LispType
  import Eval
  
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
