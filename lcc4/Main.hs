import Types
import Parser
import Beta
import Pretty
import Calculi
--------------------------------------------------------------------------------------------------------
--Main functions

run :: Int -> String -> IO()
run n s = do
  putStrLn ("\nCalculus -- " ++ showCalc n)
  putStrLn ("  String -- " ++ s)
  let e = getExpr s
  if isRight e then do
    let e' = fromRight e
    let ca = calculi !! n
    let te = typeCheck ca e'
    if isRight te then do
      putStrLn ("    Expr -- " ++ show e' ++ " : " ++ pEither te)
      let ne = normalize e'
      putStrLn ("  Normal -- " ++ show ne ++ " : " ++ pEither te ++ "\n")
      return ()
      else do
        putStrLn ("    Expr -- " ++ show e')
        putStrLn ("\n" ++ pEither te ++ "\n")
        return ()
    else do
      putStrLn ("\n" ++ pEither e ++ "\n")
      return ()

test :: Calculus -> Int --Takes a Calculus and returns the number of passed tests
test c = foldr f 0 tests
  where f test score = score + (if isRight $ typeCheck c test then 1 else 0)
        tests = map (fromRight.getExpr) testStrings

calculiTests :: IO()
calculiTests = do
  let results = map test calculi
  let expected = [1, 2, 2, 4, 2, 4, 4, 8]
  putStrLn $ "\n Results: " ++ (show results)
  putStrLn $ "Expected: " ++ (show expected)
  putStrLn $ (if results == expected then "All results as expected!" else "Some unexpected results!") ++ "\n"
  return ()
--------------------------------------------------------------------------------------------------------
--Example strings

polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat"
boolId = polyId ++ " @ Bool"

typeTypeApp = "(\\x:^a:*->*->\\y:*->x @ y)"
constBool = "(\\g:*->Bool)"

ex1 = typeTypeApp ++ " @ " ++ constBool
ex2 = polyId ++ " @ (^g:Nat->Nat)" ++ " @ (\\g:Nat->g)" ++ " @ 3"
