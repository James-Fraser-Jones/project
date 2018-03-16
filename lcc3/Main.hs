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
-}
