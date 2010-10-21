import qualified Data.Map as Map
import PerceptronDual
import Kernels

import System

toMap ((x,y),c) = (Map.fromList [("x",x),("y",y)],c)

xys = map toMap
      [((0,0),1)
      ,((0,1),-1)
      ,((1,0),-1)
      ,((1,1),1)]

eval kp i = 
    let m  = trainWithKernel kp xys i 
    in map (classify m) (map fst xys)

pts step = [ ((x,y),0) | x <- [0,step .. 1] , y <- [0,step .. 1] ]


main = do
  [k,i] <- getArgs
  let m = trainWithKernel (read k) xys (read i)
      points = (pts 0.012)
      cls = map (classify m) (map fst (map toMap points))
      xs = map (fst . fst) points
      ys = map (snd . fst) points
  putStrLn $ "x <- " ++ format xs
  putStrLn $ "y <- " ++ format ys 
  putStrLn $ "col <- " ++ format cls
  putStrLn $ "pdf('xor.pdf');plot(x,y,col=col+3,pch=20,main='XOR: Perceptron (" 
               ++ k ++ " kernel)  ');dev.off()"


format xs = "c(" ++ unwords (map (\x -> show x ++ ",") (init xs)) ++ show (last xs)  ++ ")"