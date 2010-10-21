module Kernels ( dot
               , poly
               , rbf
               , kernel
               , KernelParam (..)
               , Vector
               , Kernel
               )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
type Vector k = Map.Map k Double
type Kernel k = Vector k -> Vector k -> Double

data KernelParam n = Linear
                 | Poly n
                 | RBF Double deriving (Show,Read)

kernel :: (Ord k,Real n) => KernelParam n -> Kernel k
kernel Linear = dot
kernel (Poly d)  = poly d
kernel (RBF g)   = rbf g

dot :: (Ord k) => Kernel k
x `dot` y = if Map.size x < Map.size y then
                Map.fold (+) 0 . Map.mapWithKey (\k v -> v * Map.findWithDefault 0 k y) $ x
            else 
                Map.fold (+) 0 . Map.mapWithKey (\k v -> v * Map.findWithDefault 0 k x) $ y
 


poly :: (Ord k,Real n) =>  n -> Kernel k
poly d x x' = (1 + x `dot` x')**(realToFrac d)

rbf :: (Ord k) => Double -> Kernel k
rbf gamma x x'  =  exp (-gamma * squaredED x x')



squaredED x x' = Set.fold (\k z -> z + (Map.findWithDefault 0 k x - Map.findWithDefault 0 k x')^2) 
                          0 
                          (Set.union (Map.keysSet x) (Map.keysSet x'))
