g (S (S Z)) = Z
g x = x

main = g (S Z)
------------------------------------------------------------
-- First match fails
{-
g (S Z)
S Z
-}
