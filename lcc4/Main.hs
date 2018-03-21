import Types
import Parser
import Pretty

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

run :: String -> IO()
run s = do
  putStrLn ("\nString -- " ++ s)
  let e = getExpr s
  let te = typeCheck e
  if isRight te then do
    putStrLn ("  Expr -- " ++ show e ++ " : " ++ pError te)
    let e' = normalize e
    putStrLn ("Normal -- " ++ show e' ++ "\n")
    return ()
    else do
      putStrLn ("  Expr -- " ++ show e)
      putStrLn (pError te ++ "\n")
      return()
--------------------------------------------------------------------------------------------------------

polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat"
boolId = polyId ++ " @ Bool"
