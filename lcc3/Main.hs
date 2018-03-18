import Types
import Parser
import Pretty

polyIdType = "^a:*->^a->a" --type of polymorphic identity function for terms of type a
fmapType = "^f:(^*->*)->^a:*->^b:*->(^(^a->b)->(^(f @ a)->(f @ b)))" --fmap :: (a -> b) -> f a -> f b
fmapTypeB = "^f:^*->*->^a:*->^b:*->^^a->b->^(f @ a)->(f @ b)"
listType = "^*->*"

polyId = "(\\a:*->\\x:a->x)" --polymorphic identity function for terms of type a
natId = polyId ++ " @ Nat @ 6"
boolId = polyId ++ " @ Bool @ True"

test1 = "(λx:Nat → x)"
test3 = "(λx:Nat → [])"
test2 = "(\\a:*->\\x:a->x) @ Nat @ 6"
test4 = "(Πa:★ → Πx:a → a) @ ★ @ Nat"

{-
run' :: Expr -> IO()
run' e = do
  print e
  let b = beta e
  if b == e then return () else run' b

run :: String -> IO()
run s = do
  print s
  let e = getExpr s
  run' e

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
