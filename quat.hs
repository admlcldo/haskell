data H = One | Zero | I | J | K | Neg H | Plus H H deriving Show

qProd :: H -> H -> H
qProd Zero a = Zero
qProd One h = h
qProd I J = K
qProd J K = I
qProd K I = J
qProd (Plus a b) c = Plus (qProd a c) (qProd b c)
qProd a b = Neg (qProd b a)
qProd c c = Zero
