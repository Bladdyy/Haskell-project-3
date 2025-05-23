f (S (S x)) = x
f x = x

main = f (S (S (S Z)))
------------------------------------------------------------
-- Sprawdzam od góry
{-
f (S (S (S Z)))
S Z
-}
