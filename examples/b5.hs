f (S (S A (A x))) = a
f (A (S A (B x))) = a
f (S (S A (B x))) = b

main = f (S (S A (B x)))
------------------------------------------------------------
-- Dopasowanie do (S (S A (B x)))
{-
f (S (S A (B x)))
b
-}