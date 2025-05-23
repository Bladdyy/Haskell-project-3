zero = Z
one = S zero
two = S one
three = S two

expr1 = Add (Val two) (Mul (Val one) (Val three))

-- eval (Val n)     = n
-- eval (Add x y)   = add (eval x) (eval y)
-- eval (Mul x y)   = mul (eval x) (eval y)

eval (Val n) = n
eval (Add x y) = add (eval x) (eval y)
eval (Mul x y) = mul (eval x) (eval y)

add Z n = n
add (S m) n = S (add m n)

mul Z n = Z
mul (S m) n = add n (mul m n)

main = eval expr1
------------------------------------------------------------
{-
eval expr1
eval (Add (Val two) (Mul (Val one) (Val three)))
add (eval (Val two)) (eval (Mul (Val one) (Val three)))
add two (eval (Mul (Val one) (Val three)))
add two (mul (eval (Val one)) (eval (Val three)))
add two (mul one (eval (Val three)))
add two (mul one three)
add two (add three (mul zero three))
add two (add three Z)
add two three
S (add one three)
S (S (add zero three))
S (S three)
S (S (S two))
S (S (S (S one)))
S (S (S (S (S zero))))
-}
