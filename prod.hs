type Prod a b = (a, b)

diag :: a -> Prod a a
diag x = (x, x)

prod :: a -> b -> (Prod a b)
prod x y = (x, y)
