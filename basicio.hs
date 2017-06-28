import System.IO

main :: IO ()
main =
  do
  inh <- openFile "input" ReadMode
  mainloop inh
  hClose inh

mainloop :: Handle -> IO ()
mainloop inh =
  do
  ineof <- hIsEOF inh
  if ineof
  then
    return ()
  else
    do
    inpC <- hGetChar inh
    let l = if inpC /= '\n' then [inpC,'\n'] else [inpC]
    putStr l
    mainloop inh

    
