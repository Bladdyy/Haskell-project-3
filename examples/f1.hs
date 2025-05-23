two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
f x y = x
add (S m) = S (add m n)
main = add two two
------------------------------------------------------------
{-
    Combinators with the same names, but a different number of parameters
-}
