import Types
import Parser
import Beta
import Pretty
--------------------------------------------------------------------------------------------------------
--Main functions

run :: String -> IO()
run s = do
  putStrLn ("\nString -- " ++ s)
  let e = getExpr s
  if isRight e then do
    let e' = fromRight e
    let te = typeCheck e'
    if isRight te then do
      putStrLn ("  Expr -- " ++ show e' ++ " : " ++ pEither te)
      let ne = normalize e'
      putStrLn ("Normal -- " ++ show ne ++ "\n")
      return ()
      else do
        putStrLn ("  Expr -- " ++ show e')
        putStrLn (pEither te ++ "\n")
        return ()
    else do
      putStrLn (pEither e ++ "\n")
      return ()

main :: IO()
main = do
  putStr "HLCi: " --this reads a single backslash for lambda abstractions, and doesn't require quotes around the string
  s <- getLine
  run s
  main
--------------------------------------------------------------------------------------------------------
--Example strings

polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat"
boolId = polyId ++ " @ Bool"

typeTypeApp = "(\\x:^a:*->*->\\y:*->x @ y)"
constBool = "(\\a:*->Bool)" --this has to be a to match with the a above currently because I don't have alpha equivalence check yet
ex1 = typeTypeApp ++ " @ " ++ constBool
