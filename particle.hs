data Particle = Particle {pos :: (Double, Double), speed :: (Double, Double), acc :: (Double, Double)} deriving (Show)

bump :: Double -> Particle -> Particle
bump dt (Particle (x,y) (vx,vy) (ax,ay)) = Particle (x + dt*vx, y + dt*vy) (vx + dt*ax,vy + dt*ay) (ax, ay)

posStyle :: Particle -> String
posStyle p = show x0 ++ " " ++ show x1
  where
  (x0, x1) = pos p
