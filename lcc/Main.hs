import Types
import Parser
import Pretty
import Data.Maybe

--nameless variable for use with normal and term types
--you need to ensure that lookup correctly chooses the most recent binding of a variable from the list
--app does not (and should not) interact with deps, the only purpose of dep is to represent the type of a lam

{-
getType :: E -> Maybe E
--types of literal terms are the types of those terms
getType (Lit (B _)) = Just $ LitT Bool
getType (Lit (I _)) = Just $ LitT Int
--type of literal types is kind * (but so is the type of functions from type to type, which means that dep needs to reflect this)
getType (LitT _) = Just Star
--type of star is box (but so is the type of functions from star to star, which means that dep needs to reflect this)
getType Star = Just Box
--type of box is nothing
getType Box = Nothing --there is no type of a type of a kind
-}

--termToTerm :: E --the TYPE of a function from terms to terms
--termToTerm = Dep "_" a b where (type a, type b) = (Star, Star)

--typeToType :: E --the TYPE of a function from types to types
--typeToType = Dep "_" a b where (type a, type b) = (Box, Box)
