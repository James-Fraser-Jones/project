module Calculi (calculi, testStrings, getAbsForms) where
import Types
--------------------------------------------------------------------------------------------------------
{- VV = Value to Value function, TV = Type to Value function
   TT = Type to Type function,   VT = Value to Type function -}
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

testStrings :: [String]
testStrings = [t0,t1,t2,t3,t4,t5,t6,t7]

calculi :: [Calculus]
calculi = [S, SP, ST, SD, SPT, SPD, STD, SPTD]

getAbsForms :: Calculus -> AbsForms
getAbsForms c = case c of
  S ->    [(Star,Star)]
  SP ->   [(Star,Star),(Box,Star)]
  ST ->   [(Star,Star),(Box,Box)]
  SD ->   [(Star,Star),(Star,Box)]
  SPT ->  [(Star,Star),(Box,Star),(Box,Box)]
  STD ->  [(Star,Star),(Box,Box),(Star,Box)]
  SPD ->  [(Star,Star),(Box,Star),(Star,Box)]
  SPTD -> [(Star,Star),(Box,Star),(Box,Box),(Star,Box)]
