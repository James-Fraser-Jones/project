import Types
import Parser
import Beta
import Pretty
--------------------------------------------------------------------------------------------------------
--Instances of specific calculi

getCalc :: Int -> Calculus
getCalc n = (check n 8 (Star, Box)) ++ (check n 4 (Box, Box)) ++ (check n 2 (Box, Star)) ++ [(Star, Star)]
  where check n k c = if (n `mod` k) >= (k `div` 2) then [c] else []

c0, c1, c2, c3, c4, c5, c6, c7 :: Calculus
c0 = getCalc 0 --simply typed lambda calculus, (λ→)
c1 = getCalc 1 --second order lambda calculus, system f (λ2)
c2 = getCalc 2 --weak (λω)
c3 = getCalc 3 --system f omega (λω)
c4 = getCalc 4 --logical framework (λP)
c5 = getCalc 5 --(λP2)
c6 = getCalc 6 --weak (λPω)
c7 = getCalc 7 --calculus of constructions, (λC or λPω)

calculi :: [Calculus]
calculi = [c0, c1, c2, c3, c4, c5, c6, c7]
--------------------------------------------------------------------------------------------------------
--Main functions

run :: Calculus -> String -> IO()
run ca s = do
  putStrLn ("\nString -- " ++ s)
  let e = getExpr s
  if isRight e then do
    let e' = fromRight e
    let te = typeCheck ca e'
    if isRight te then do
      putStrLn ("  Expr -- " ++ show e' ++ " : " ++ pEither te)
      let ne = normalize e'
      putStrLn ("Normal -- " ++ show ne ++ " : " ++ pEither te ++ "\n")
      return ()
      else do
        putStrLn ("  Expr -- " ++ show e')
        putStrLn (pEither te ++ "\n")
        return ()
    else do
      putStrLn (pEither e ++ "\n")
      return ()

runTests :: IO()
runTests = do
  let scores = map (flip test tests) calculi
  if scores == expected then putStrLn "All tests passed!" else putStrLn "Some tests failed!"

--------------------------------------------------------------------------------------------------------
--Example strings

polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat"
boolId = polyId ++ " @ Bool"

typeTypeApp = "(\\x:^a:*->*->\\y:*->x @ y)"
constBool = "(\\a:*->Bool)" --this has to be a to match with the a above currently because I don't have alpha equivalence check yet

ex1 = typeTypeApp ++ " @ " ++ constBool
ex2 = polyId ++ " @ (^g:Nat->Nat)" ++ " @ (\\g:Nat->g)" ++ " @ 3"
--------------------------------------------------------------------------------------------------------
--Tests

test0 = "(\\x:Nat -> x) @ 4" --VV
test1 = "(\\x:*   ->   x) @ Nat" --TT
test2 = "(\\x:*   ->   4) @ Nat" --TV
test3 = "(\\x:Nat -> Nat) @   4" --VT
test4 = "(\\x:^c:*->*   -> \\y:*   ->   4) @ (\\c:* -> c) @ Nat" --TT and TV
test5 = "(\\x:^c:*->*   -> \\y:Nat -> Nat) @ (\\c:* -> c) @ 4"   --TT and VT
test6 = "(\\x:^c:*->Nat -> \\y:Nat -> Nat) @ (\\c:* -> 4) @ 4"   --TV and VT
test7 = "(\\x:^c:*->* -> \\y:^d:*->Nat -> \\z:Nat -> Nat) @ (\\c:* -> c) @ (\\d:* -> 4) @ 4" --TT and TV and VT

tests :: [String]
tests = [test0, test1, test2, test3, test4, test5, test6, test7]

test :: Calculus -> [String] -> Int
test c = foldr f 0
  where f test score = score + (if isRight $ typeCheck c (fromRight (getExpr test)) then 1 else 0)

expected :: [Int]
expected = [1, 2, 2, 4, 2, 4, 4, 8]
