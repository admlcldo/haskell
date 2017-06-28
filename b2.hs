import System.IO
import Data.Char


returnTest :: IO ()
returnTest =
  do
  one <- return 1
  let two = 2
  putStrLn $ show (one + two)
