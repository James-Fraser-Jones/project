import Types
import Parser
import Beta
import Pretty
import Calculi
--------------------------------------------------------------------------------------------------------
--Main functions

coc :: String -> IO ()
coc = run 7

run :: Int -> String -> IO ()
run n s = do
  putStrLn $ "\n  Calc -- " ++ (showCalc n)
  let vals = getVals n s
  if isRight vals then success $ fmap show (fromRight vals) else (err.show.fromLeft) vals

success :: [String] -> IO ()
success [e, t, e', d] = do
  putStrLn $ "  Expr -- " ++ e ++ " : " ++ t
  putStrLn $ "  Beta -- " ++ e' ++ " : " ++ t
  putStrLn $ " Delta -- " ++ d ++ " : " ++ t ++ "\n"
  return ()

err :: String -> IO ()
err e = do
  putStrLn $ " Error -- " ++ e ++ "\n"
  return ()

getVals :: Int -> String -> Either Error [Expr]
getVals n s = do
  e <- getExpr s
  let c = calculi !! n
  t <- typeCheck c e
  let e' = normalize e
  let d = delta t e'
  return [e, t, e', d]

test :: Calculus -> Int --Takes a Calculus and returns the number of passed tests
test c = foldr f 0 tests
  where f test score = score + (if isRight $ typeCheck c test then 1 else 0)
        tests = map (fromRight.getExpr) testStrings

calculiTests :: IO()
calculiTests = do
  let results = map test calculi
  let expected = [1, 2, 2, 4, 2, 4, 4, 8]
  putStrLn $ "\n  Results -- " ++ (show results)
  putStrLn $ " Expected -- " ++ (show expected)
  putStrLn $ (if results == expected then " All results as expected!" else " Some unexpected results!") ++ "\n"
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

exIf = "If @ Nat @ True @ 6 @ 4"
exPlus = "+ @ 4 @ 7"
exAnd = "& @ True @ False"
exLit = "(\\b:Bool -> \\a:Nat -> \\c:Nat -> + @ (If @ Nat @ (& @ b @ False) @ a @ c) @ 4) @ True @ 3 @ 2"

escapesTypeChecking = "If @ Nat @ True @ 3 @ 7" --these two strings have the same behavior but the first escapes typechecking
noEscape = "((\\t:* -> \\b:Bool -> \\x:t -> \\y:t -> If @ t @ b @ x @ y) @ Nat @ True @ 3 @ 7)" --this won't work without polymorphism
