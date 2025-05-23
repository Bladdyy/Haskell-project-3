f (S (S x)) = x
f x = x

main = f (S (S (S Z)))
------------------------------------------------------------
-- Check first function first
{-
f (S (S (S Z)))
S Z
-}
