data Vec =
  Vec {
  xComp :: Double,
  yComp :: Double,
  zComp :: Double}

(^+^) :: Vec -> Vec -> Vec
Vec x y z ^+^ Vec x' y' z' = Vec (x + x') (y + y') (z + z')  
