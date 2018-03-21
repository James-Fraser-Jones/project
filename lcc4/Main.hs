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
fmapType = "^f:(^*->*)->^a:*->^b:*->(^(^a->b)->(^(f @ a)->(f @ b)))" --fmap :: (a -> b) -> f a -> f b
fmapTypeB = "^f:^*->*->^a:*->^b:*->^^a->b->^(f @ a)->(f @ b)"

polyIdType = "^a:*->^a->a" --type of polymorphic identity function for terms of type a
polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat @ 6"
boolId = polyId ++ " @ Bool @ True"

test1 = "(λx:Nat → x)"
test3 = "(λx:Nat → [])"
test2 = "(\\a:*->\\x:a->x) @ Nat @ 6"
test4 = "(Πa:★ → Πx:a → a) @ ★ @ Nat"
--------------------------------------------------------------------------------------------------------
{-
Basically we have to think carefully:

(Πa:★ → Πx:a → a) ★ Nat
this needs to beta reduce to
Nat

Because it is the type of
(λa:★ → λx:a → x) Nat 6
which beta reduces to
6

Actually thinking about it for simplicity we might want to always represent in compact form:
(Πa:★ → Πa → a) ★ Nat
But I can't figure out how this should be done really.
You just need to know whether there are still any references to the variable x when you find the type of the last expression

Maybe you don't actually replace literals with their types:

(Πa:★ → Πx:a → a) Nat 6
=
(Πx:Nat → Nat) 6
=
Nat
-}
