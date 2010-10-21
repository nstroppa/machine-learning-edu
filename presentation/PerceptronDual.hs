module PerceptronDual ( train
                      , classify 
                      , trainWithKernel
                      , Kernel
                      , Model
                      , Example
                      )
where
import Data.List 
import qualified Data.Map as Map
import Kernels (Vector, Kernel, KernelParam(..), kernel)


type Model k n = (Alphas k,Double,KernelParam n)
type Example k = (Vector k,Int)
type Alphas k = Map.Map (Example k) Double



--train :: (Ord k) => [Example k] -> Int -> Model
train xs i = averagedPerceptron Linear xs i

--trainWithKernel :: KernelParam -> [Example] -> Int -> Model
trainWithKernel kp xs i = averagedPerceptron kp xs i

--classify :: Model -> Vector -> Int 
classify (alphas,bias,kp) x = if score k alphas x bias > 0 then 1 else -1
    where k = kernel kp


v `plus` w  = Map.unionWith (+) v w

n `times` w = Map.map (*n) w
w `over` n  = Map.map (/n) w



data Params k = Params !Double (Alphas k) !Double (Alphas k) !Double
averagedPerceptron kp xys i = 
    let params  = Params 0.0 Map.empty 0 Map.empty 0
        p@(Params c w_0 b_0 w_a b_a) = foldl' step params [ xy |  j <- [1..i], xy <- xys ]
    in (w_0 `minus` (w_a `over` c), b_0 - b_a/c, kp)
    where step (Params c w_0 b_0 w_a b_a) (x,y) = 
            if fromIntegral y * score k w_0 x b_0 <= 0 then
               Params (c + 1)
                      (Map.unionWith (+) w_0 (Map.singleton (x,y) 1))
                      (b_0 + fromIntegral y)
                      (Map.unionWith (+) w_a (Map.singleton (x,y) c))
                      (b_a + c* fromIntegral y)
            else Params (c + 1) w_0 b_0 w_a b_a
          v `minus` w = Map.unionWith (-) v w
          -- only valid if keys v == keys w !!!
          k = kernel kp


score kern alphas x b = Map.foldWithKey (\(x',y') alpha z -> z + (alpha * fromIntegral y' * (x' `kern` x) + b)) 0 alphas