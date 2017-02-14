module LinearSize0or1 where

import NaturalSize
import TermUnranking
import Affine

-- ============================================================
--         COUNTING WITH VARIABLE SIZE 0
-- ============================================================
-- It is not about natural size by I put it here for convenience

-- Constants

memoryL0 :: Int -> [Int] -> Mem
memoryL0 0 m = Load [lm0 n (reverse m) | n<-[0..]]
memoryL0 k m = Mem [memoryL0 (k-1) (j:m) | j<-[0..]] 

theMemoryL0 = memoryL0 upBound []

accL0 :: Int -> [Int] -> Integer
accL0 n m = access theMemoryL0 n m

-- counting affine terms that are applications
lm0App n m  = sum (map (\((q,r),(k,nk))->(accL0 k q)*(accL0 nk r)) (allCombinations m (n-1)))

-- counting affine terms that are abstractions with binding at depth i
lm0ABSAtD n m i = (fromIntegral(1 + m!!i))*(accL0 (n-1) (tail (inc i m)++[0])) -- à voir

-- counting affine terms that are abstractions with binding
lm0ABSwB :: Int -> [Int] -> Integer
lm0ABSwB n m
  | head m == 0 = sum [lm0ABSAtD n m i |i<-[1..n]]
  | otherwise = 0

lm0 :: Int -> [Int] -> Integer
lm0 0 m = iv (head m == 1 && all ((==) 0) (tail m)) -- there is only □0
lm0 n m = lm0App n m + lm0ABSwB n m 

nbClosedLinearSize0 = [lm0 n (replicate upBound 0) | n<-[0..upBound]]

-- [0,1,0,5,0,60,0,1105,0,27120,0,828250, ...

-- ============================================================
--         COUNTING WITH VARIABLE SIZE 1
-- ============================================================
-- It is not about natural size by I put it here for convenience

-- Constants

memoryL1 :: Int -> [Int] -> Mem
memoryL1 0 m = Load [lm1 n (reverse m) | n<-[0..]]
memoryL1 k m = Mem [memoryL1 (k-1) (j:m) | j<-[0..]] 

theMemoryL1 = memoryL1 upBound []

accL1 :: Int -> [Int] -> Integer
accL1 n m = access theMemoryL1 n m

-- counting affine terms that are applications
lm1App n m  = sum (map (\((q,r),(k,nk))->(accL1 k q)*(accL1 nk r)) (allCombinations m (n-1)))

-- counting affine terms that are abstractions with binding at depth i
lm1ABSAtD n m i = (fromIntegral(1 + m!!i))*(accL1 (n-2) (tail (inc i m)++[0]))

-- counting affine terms that are abstractions with binding
lm1ABSwB :: Int -> [Int] -> Integer
lm1ABSwB n m
  | head m == 0 = sum [lm1ABSAtD n m i |i<-[1..n]]
  | otherwise = 0
 
lm1 :: Int -> [Int] -> Integer
lm1 0 m = iv (head m == 1 && all ((==) 0) (tail m)) -- there is only □0
lm1 1 m = iv (head m == 2 && all ((==) 0) (tail m)) -- there is only □0 □0 
lm1 n m = lm1App n m + lm1ABSwB n m 

nbClosedLinearSize1 = [lm1 n (replicate upBound 0) | n<-[0..upBound]]

-- sequence A062980
nbClosedLinearSize1Step3 = [lm1 (3*n+2) (replicate upBound 0) | n<-[0..]]
--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
