data E = X | One | Zero | Negate E | Sum E E | Product E E | Exp E deriving (Show)

eval :: Double -> E -> Double
eval x = ev where
  ev arg = case arg of
    X            -> x
    One          -> 1
    Zero         -> 0
    Negate e     -> -ev e
    Sum e e'     -> ev e + ev e'
    Product e e' -> ev e * ev e'
    Exp e -> exp (ev e)

diff :: E -> E
diff arg = case arg of
  X -> One
  One -> Zero
  Zero -> Zero
  Negate e -> Negate (diff e)
  Sum e e' -> Sum (diff e) (diff e')
  Product e e' -> Sum (Product (diff e) e') (Product e (diff e'))
  Exp e -> Product (Exp e) (diff e)

diffEval :: Double -> E -> (Double, Double)
diffEval x = ev where
  ev arg = case arg of
    X -> (x, 1)
    One -> (1, 0)
    Zero -> (0, 0)
    Negate e ->
      let (a,a') = ev e in (-a, -a')
    Sum e f ->
      let
      (a, a') = ev e
      (b, b') = ev f
      in (a+b, a' + b')
    Product e f ->
      let
      (a, a') = ev e
      (b, b') = ev f
      in (a * b, a' * b + a * b')
    Exp e ->
      let
      (a, a') = ev e
      in (exp a, (exp a) * a')


f = iterate (\e -> Exp (Sum One (Negate e))) X !! 5000

number = 1.5
test1 = diffEval number f
test2 = eval number (diff f)
