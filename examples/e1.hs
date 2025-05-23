one = S Z
two = S one
add Z n = n
add (S m) n = S (add m n)
main = add two two


----------------------------
-- {main}
-- {add two two}
-- add {S one} two
-- {S (add one two)}
-- S (add {S Z} two)
-- S {S (add Z two)}
-- S (S {two})
-- S (S {S one})
-- S (S (S {S Z}))
