module LinearSize0or1 where

import NaturalSize
import TermUnranking
import Affine

-- ============================================================
--         COUNTING WITH VARIABLE SIZE 0
-- ============================================================
-- It is not about natural size by I put it here for convenience

-- Constants

memoryA0 :: Int -> [Int] -> Mem
memoryA0 0 m = Load [am0 n (reverse m) | n<-[0..]]
memoryA0 k m = Mem [memoryA0 (k-1) (j:m) | j<-[0..]] 

theMemoryA0 = memoryA0 upBound []

accA0 :: Int -> [Int] -> Integer
accA0 n m = access theMemoryA0 n m

-- counting affine terms that are applications
am0App n m  = sum (map (\((q,r),(k,nk))->(accA0 k q)*(accA0 nk r)) (allCombinations m (n-1)))

-- counting affine terms that are abstractions with binding at depth i
am0ABSAtD n m i = (fromIntegral(1 + m!!i))*(accA0 (n-1) (tail (inc i m)++[0])) 
-- counting affine terms that are abstractions with binding
am0ABSwB :: Int -> [Int] -> Integer
am0ABSwB n m
  | head m == 0 = sum [am0ABSAtD n m i |i<-[1..n]]
  | otherwise = 0

-- counting affine terms that are abstractions with no binding
am0ABSnB :: Int -> [Int] -> Integer
am0ABSnB n m 
    | head m == 0 = (accA0 (n-1) (tail m ++ [0]))
    | otherwise = 0

am0 :: Int -> [Int] -> Integer
am0 0 m = iv (head m == 1 && all ((==) 0) (tail m))
am0 n m = am0App n m + am0ABSwB n m + am0ABSnB n m 

nbClosedAffineSize0 = [am0 n (replicate upBound 0) | n<-[0..upBound]]

-- ============================================================
--         COUNTING WITH VARIABLE SIZE 1
-- ============================================================
-- It is not about natural size by I put it here for convenience

-- Constants

memoryA1 :: Int -> [Int] -> Mem
memoryA1 0 m = Load [am1 n (reverse m) | n<-[0..]]
memoryA1 k m = Mem [memoryA1 (k-1) (j:m) | j<-[0..]] 

theMemoryA1 = memoryA1 upBound []

accA1 :: Int -> [Int] -> Integer
accA1 n m = access theMemoryA1 n m

-- counting affine terms that are applications
am1App n m  = sum (map (\((q,r),(k,nk))->(accA1 k q)*(accA1 nk r)) (allCombinations m (n-1)))

-- counting affine terms that are abstractions with binding at depth i
am1ABSAtD n m i = (fromIntegral(1 + m!!i))*(accA1 (n-2) (tail (inc i m)++[0])) -- à voir

-- counting affine terms that are abstractions with binding
am1ABSwB :: Int -> [Int] -> Integer
am1ABSwB n m
  | head m == 0 = sum [am1ABSAtD n m i |i<-[1..n]]
  | otherwise = 0
 
-- counting affine terms that are abstractions with no binding
am1ABSnB :: Int -> [Int] -> Integer
am1ABSnB n m 
    | head m == 0 = (accA1 (n-1) (tail m ++ [0]))
    | otherwise = 0

am1 :: Int -> [Int] -> Integer
am1 0 m = iv (head m == 1 && all ((==) 0) (tail m)) -- there is only □0
am1 1 m = iv (head m == 2 && all ((==) 0) (tail m)) -- there is only □0 □0 
am1 n m = am1App n m + am1ABSwB n m + am1ABSnB n m

nbClosedAffineSize1 = [am1 n (replicate upBound 0) | n<-[0..upBound]]

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
