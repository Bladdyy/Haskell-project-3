zero = Z
one = S Z
two = S one
three = S two
four = S three

add Z n = n
add (S m) n = S (add m n)

main = add two three
------------------------------------------------------------
{-
add two three
S (add one three)
S (S (add Z three))
S (S three)
S (S (S two))
S (S (S (S one)))
S (S (S (S (S Z))))
-}
