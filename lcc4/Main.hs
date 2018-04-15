import Types
import Parser
import Beta
import Pretty
import Calculi
--------------------------------------------------------------------------------------------------------
--Main functions

coc = SPTD

{-
respond :: Calculus -> String -> IO Calculus
respond c s = let

loop :: Calculus -> IO()
loop c = do
  putStr "HLCi> "
  s <- getLine
  if s == "exit" then return ()
    else do
      nc <- respond c s
      loop nc
-}

run :: Calculus -> String -> IO ()
run c s = do
  putStrLn $ "\n  Calc -- " ++ (show c)
  let vals = getVals c s
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

getVals :: Calculus -> String -> Either Error [Expr]
getVals c s = do
  e <- getExpr s
  t <- typeCheck (getAbsForms c) e
  let e' = normalize e
  let d = delta t e'
  return [e, t, e', d]

test :: Calculus -> Int --Takes a Calculus and returns the number of passed tests
test c = foldr f 0 tests
  where f test score = score + (if isRight $ typeCheck (getAbsForms c) test then 1 else 0)
        tests = map (fromRight.getExpr) testStrings

calculiTests :: IO()
calculiTests = do
  let results = map test calculi
  let expected = [1, 2, 2, 2, 4, 4, 4, 8]
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

exPlus = "(\\x:Nat -> \\y:Nat -> + @ x @ y) @ 4 @ 13"
exAnd = "(\\x:Bool -> \\y:Bool -> & @ x @ y) @ True @ False"
