import Types

import Parser
import Printer

import Beta
import Calculi

import System.IO

import Text.Read
import Data.Maybe
--------------------------------------------------------------------------------------------------------
--Top level functions

defaultCalc = SPTD --Default Calculus is the CoC

main :: IO()
main = do
  hSetBuffering stdout NoBuffering --force HLCI prompt to appear on the same line as user input
  putStrLn $ "\n Type \"Help\" for help, \"Tests\" to run the calculus tests or \"Exit\" to exit the input loop"
  putStrLn $ " Calc set to: " ++ pCalc defaultCalc ++ "\n"
  loop defaultCalc

run :: Calculus -> String -> IO ()
run c s = do
  putStrLn $ ""
  let vals = getVals c s
  if isRight vals then success $ fmap show (fromRight vals) else (err.show.fromLeft) vals

runTests :: IO()
runTests = do
  putStr("\n")
  testCalculi calculi expected

help :: IO()
help = (putStrLn "\n Type a valid lambda expression or type one of the following calculi to switch the type system:") >> (mapM_ putStrLn helpStrings) >> (putStrLn "")
--------------------------------------------------------------------------------------------------------
--Example strings (only use a single backslash for lambda when using compiled execuatable)

polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat"
boolId = polyId ++ " @ Bool"

typeTypeApp = "(\\x:^a:*->*->\\y:*->x @ y)"
constBool = "(\\g:*->Bool)"

ex1 = typeTypeApp ++ " @ " ++ constBool
ex2 = polyId ++ " @ (^g:Nat->Nat)" ++ " @ (\\g:Nat->g)" ++ " @ 3"

exPlus = "(\\x:Nat -> \\y:Nat -> + @ x @ y) @ 4 @ 13"
exAnd = "(\\x:Bool -> \\y:Bool -> & @ x @ y) @ True @ False"

exCapture = "\\y:Nat -> ((\\x:Nat -> \\y:Nat -> x) @ y @ 2)"
--------------------------------------------------------------------------------------------------------
--Running

helpStrings :: [String]
helpStrings = map (" " ++) (zipWith (++) (map show calculi) (map ((" -- " ++).pCalc) calculi))

loop :: Calculus -> IO()
loop calc = do
  putStr "HLCi> "
  input <- getLine
  let newCalc = (readMaybe input) :: Maybe Calculus
  if isJust newCalc then (putStrLn $ "\n Calc set to: " ++ pCalc (fromJust newCalc) ++ "\n") >> (loop $ fromJust newCalc)
    else case input of
      ""      -> loop calc
      "Help"  -> help >> loop calc
      "Tests" -> runTests >> loop calc
      "Exit"  -> putStrLn "\n Exiting loop" >> return ()
      _       -> run calc input >> loop calc

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
--------------------------------------------------------------------------------------------------------
--Testing Calculi

tests :: [Expr]
tests = map (fromRight.getExpr) testStrings

expected :: [[Bool]]
expected =
  [[ True,False,False,False,False,False,False,False],
   [ True,False, True,False,False,False,False,False],
   [ True, True,False,False,False,False,False,False],
   [ True,False,False, True,False,False,False,False],
   [ True, True, True,False, True,False,False,False],
   [ True,False, True, True,False,False, True,False],
   [ True, True,False, True,False, True,False,False],
   [ True, True, True, True, True, True, True, True]]

testCalculi :: [Calculus] -> [[Bool]] -> IO()
testCalculi [] _ = return ()
testCalculi (c:cs) (e:es) = do
  test c e
  testCalculi cs es

test :: Calculus -> [Bool] -> IO()
test c e = do
  putStrLn $ pCalc c
  let results = map (\test -> (if isRight $ typeCheck (getAbsForms c) test then True else False)) tests
  putStr $ show results
  putStrLn $ " - " ++ (if results == e then "Correct" else "Incorrect") ++ "\n"

captureTest :: Expr
captureTest = beta (fromRight (getExpr exCapture)) --demonstrates capture avoidance
