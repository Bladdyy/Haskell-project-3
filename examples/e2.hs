zero = Z
one = S zero
two = S one
three = S two

add Z n = n
add (S m) n = S (add m n)

mul Z n = Z
mul (S m) n = add n (mul m n)

main = mul two three
------------------------------------------------------------
{-
mul two three
add three (mul one three)
add three (add three (mul zero three))
add three (add three Z)
add three three
S (add two three)
S (S (add one three))
S (S (S (add zero three)))
S (S (S three))
S (S (S (S two)))
S (S (S (S (S one))))
S (S (S (S (S (S zero)))))
-}