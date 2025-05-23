two = S (S Z)
add Z n = n
add (S S m) n = S (add m n)
add (W m) n = S (add m n)
main = add two two
------------------------------------------------------------
-- Brak dopasowania
{-
zadanie3: non-exhaustive pattern matches
-}