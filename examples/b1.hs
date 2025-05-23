two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
-- Przykład z polecenia dla poziomu1
{-
add two two
S (add (S Z) two)
S (S (add Z two))
S (S two)
S (S (S (S Z)))
-}