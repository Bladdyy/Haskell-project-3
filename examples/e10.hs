swap (Pair x y) = Pair y x
main = swap (Pair (S Z) Z)
------------------------------------------------------------
{-
swap (Pair (S Z) Z)
Pair Z (S Z)
-}
