unwrap (Box x) = x
id x = x
main = unwrap (id (Box (S (S Z))))
------------------------------------------------------------
{-
unwrap (id (Box (S (S Z))))
S (S Z)
-}
