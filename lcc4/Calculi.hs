module Calculi (calculi, testStrings) where
import Types
--------------------------------------------------------------------------------------------------------
--Calculus generation and tests for them

{-
getCalc 0 --simply typed lambda calculus (λ→), getCalc 1 --second order lambda calculus, system f (λ2)
getCalc 2 --weak (λω),                         getCalc 3 --system f omega (λω)
getCalc 4 --logical framework (λP),            getCalc 5 --(λP2)
getCalc 6 --weak (λPω),                        getCalc 7 --calculus of constructions (λPω or λC)
-}
getCalc :: Int -> Calculus
getCalc n = (check n 8 (Star, Box)) ++ (check n 4 (Box, Box)) ++ (check n 2 (Box, Star)) ++ [(Star, Star)]
  where check n k c = if (n `mod` k) >= (k `div` 2) then [c] else []

{-
VV = Value to Value function, TV = Type to Value function
TT = Type to Type function,   VT = Value to Type function
-}
t0, t1, t2, t3, t4, t5, t6, t7 :: String
t0 = "(\\x:Nat -> x) @ 4" --VV
t1 = "(\\x:*   ->   x) @ Nat" --TT
t2 = "(\\x:*   ->   4) @ Nat" --TV
t3 = "(\\x:Nat -> Nat) @   4" --VT
t4 = "(\\x:^c:*->*   -> \\y:*   ->   4) @ (\\a:* -> a) @ Nat" --TT and TV
t5 = "(\\x:^c:*->*   -> \\y:Nat -> Nat) @ (\\a:* -> a) @ 4"   --TT and VT
t6 = "(\\x:^c:*->Nat -> \\y:Nat -> Nat) @ (\\a:* -> 4) @ 4"   --TV and VT
t7 = "(\\x:^c:*->* -> \\y:^d:*->Nat -> \\z:Nat -> Nat) @ (\\a:* -> a) @ (\\b:* -> 4) @ 4" --TT and TV and VT
--------------------------------------------------------------------------------------------------------
--Top level functions

calculi :: [Calculus]
calculi = map getCalc [0..7]

testStrings :: [String]
testStrings = [t0,t1,t2,t3,t4,t5,t6,t7]
