-- This modules deals with counting and generating
-- closed linear normal forms for the variable size 0 or the variable size 1
module LinearClosedNormalFormSize0or1 where

import NaturalSize
import System.Random
import TermUnranking
import Affine
import NormalForm
import SwissCheese

-----------------------------------------------------
--  counting with variable size  0
-----------------------------------------------------
-- The memory for storing the computation of lmCNF0
memoryLCNF0 :: Int -> [Int] -> Mem
memoryLCNF0 0 m = Load [lmCNF0 n (reverse m) | n<-[0..]]
memoryLCNF0 k m = Mem [memoryLCNF0 (k-1) (j:m) | j<-[0..]] 

theMemoryLCNF0 = memoryLCNF0 (upBound) []

accLCNF0 n m = access theMemoryLCNF0 n m

-- memory of neutrals0

memoryLNeutrals0 :: Int -> [Int] -> Mem
memoryLNeutrals0 0 m = Load [lmNeutrals0 n (reverse m) | n<-[0..]]
memoryLNeutrals0 k m = Mem [memoryLNeutrals0 (k-1) (j:m) | j<-[0..]] 

theMemoryLNeutrals0 = memoryLNeutrals0 (upBound) []

accLNeutrals0 :: Int -> [Int] -> Integer
accLNeutrals0 n m = access theMemoryLNeutrals0 n m

-- Neutrals0
lmNeutrals0 :: Int -> [Int] -> Integer
lmNeutrals0 0 m = iv (head m == 1 && all ((==) 0) (tail m))
                  -- there is only □0
lmNeutrals0 n m = sum (map (\((q,r),(k,nk))->(accLNeutrals0 k q)*(accLCNF0 nk r)) (allCombinations m (n-1))) 

-- counting affine terms that are abstractions with binding at depth i
lmLCNF0ABSAtD n m i = (fromIntegral (1 + m!!i))*(accLCNF0 (n - 1) ((tail (inc i m)) ++ [0]))

-- counting affine terms that are abstractions with binding
lmLCNF0ABSwB :: Int -> [Int] -> Integer
lmLCNF0ABSwB n m 
  | head m == 0 = sum [lmLCNF0ABSAtD n m i |i<-[1..n]]
  | otherwise = 0
                
lmCNF0 :: Int -> [Int] -> Integer
lmCNF0 0 m = lmNeutrals0 0 m
lmCNF0 n m =  lmNeutrals0 n m + lmLCNF0ABSwB n m

listNbLCNF0 = [lmCNF0 n (replicate upBound 0)|n<-[0..upBound]]
listNbLCNF0By2 =  [lmCNF0 (2*k+1) (replicate upBound 0)|k<-[0..(upBound `div` 2)]]

list_n_and_nbLCNF0 = [(n,lmCNF0 n (replicate upBound 0))|n<-[0..upBound]]


-----------------------------------------------------
--  counting with variable size  1
-----------------------------------------------------
-- The memory for storing the computation of lmCNF1
memoryLCNF1 :: Int -> [Int] -> Mem
memoryLCNF1 0 m = Load [lmCNF1 n (reverse m) | n<-[0..]]
memoryLCNF1 k m = Mem [memoryLCNF1 (k-1) (j:m) | j<-[0..]] 

theMemoryLCNF1 = memoryLCNF1 (upBound) []

accLCNF1 n m = access theMemoryLCNF1 n m

-- memory of neutrals1

memoryLNeutrals1 :: Int -> [Int] -> Mem
memoryLNeutrals1 0 m = Load [lmNeutrals1 n (reverse m) | n<-[0..]]
memoryLNeutrals1 k m = Mem [memoryLNeutrals1 (k-1) (j:m) | j<-[0..]] 

theMemoryLNeutrals1 = memoryLNeutrals1 (upBound) []

accLNeutrals1 :: Int -> [Int] -> Integer
accLNeutrals1 n m = access theMemoryLNeutrals1 n m

-- Neutrals1
lmNeutrals1 :: Int -> [Int] -> Integer
lmNeutrals1 0 m = iv (head m == 1 && all ((==) 0) (tail m))
                  -- there is only □0
lmNeutrals1 1 m = iv (head m == 2 && all ((==) 0) (tail m))
                  -- there is only □0 □0
lmNeutrals1 n m = sum (map (\((q,r),(k,nk))->(accLNeutrals1 k q)*(accLCNF1 nk r)) (allCombinations m (n-1))) 

-- counting affine terms that are abstractions with binding at depth i
lmLCNF1ABSAtD n m i = (fromIntegral (1 + m!!i))*(accLCNF1 (n - 2) ((tail (inc i m)) ++ [0]))

-- counting affine terms that are abstractions with binding
lmLCNF1ABSwB :: Int -> [Int] -> Integer
lmLCNF1ABSwB n m 
  | head m == 0 = sum [lmLCNF1ABSAtD n m i |i<-[1..n]]
  | otherwise = 0
                
lmCNF1 :: Int -> [Int] -> Integer
lmCNF1 0 m = lmNeutrals1 0 m
lmCNF1 1 m = lmNeutrals1 1 m
lmCNF1 n m =  lmNeutrals1 n m + lmLCNF1ABSwB n m

listNbLCNF1 = [lmCNF1 n (replicate upBound 0)|n<-[0..upBound]]
listNbLCNF1By3 =  [lmCNF1 (3*k+2) (replicate upBound 0)|k<-[0..(upBound `div` 3)]]

list_n_and_nbLCNF1 = [(n,lmCNF1 n (replicate upBound 0))|n<-[0..upBound]]

-- ==============================
--  GENERATION
-- ==============================
-----------------------------------------------------
--  generating with variable size  0
-----------------------------------------------------
-- The memory for storing the computation of lgCNF0
memorySCLCNF0 :: Int -> [Int] -> MemSC
memorySCLCNF0 0 m = LoadSC [lgCNF0 n (reverse m) | n<-[0..]]
memorySCLCNF0 k m = MemSC [memorySCLCNF0 (k-1) (j:m) | j<-[0..]] 

theMemorySCLCNF0 = memorySCLCNF0 (upBound) []

accSCLCNF0 n m = accessSC theMemorySCLCNF0 n m

-- memory of neutrals0

memorySCLNeutrals0 :: Int -> [Int] -> MemSC
memorySCLNeutrals0 0 m = LoadSC [lgNeutrals0 n (reverse m) | n<-[0..]]
memorySCLNeutrals0 k m = MemSC [memorySCLNeutrals0 (k-1) (j:m) | j<-[0..]] 

theMemorySCLNeutrals0 = memorySCLNeutrals0 (upBound) []

accSCLNeutrals0 :: Int -> [Int] -> [SwissCheese]
accSCLNeutrals0 n m = accessSC theMemorySCLNeutrals0 n m

-- Neutrals0
lgNeutrals0 :: Int -> [Int] -> [SwissCheese]
lgNeutrals0 0 m = if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
lgNeutrals0 n m =
  foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accSCLNeutrals0 k q)
                                                        (accSCLCNF0 nk r))
                                              (allCombinations m (n-1)))

-- generating linear terms that are abstractions with binding at depth i
lgLCNF0ABSAtD n m i = foldr (++) [] (map (abstract (i-1)) (accSCLCNF0 (n - 1)
                                                      (tail (inc i m) ++ [0])))

-- generating linear terms that are abstractions with binding
lgLCNF0ABSwB :: Int -> [Int] -> [SwissCheese]
lgLCNF0ABSwB n m 
  | head m == 0 = foldr (++) [] [lgLCNF0ABSAtD n m i |i<-[1..n]]
  | otherwise = []
                
lgCNF0 :: Int -> [Int] -> [SwissCheese]
lgCNF0 0 m = lgNeutrals0 0 m
lgCNF0 n m =  lgNeutrals0 n m ++ lgLCNF0ABSwB n m

listSCLCNF0 = [lgCNF0 n (replicate upBound 0)|n<-[0..upBound]]
listSCLCNF0By2 =  [lgCNF0 (2*k+1) (replicate upBound 0)|k<-[0..(upBound `div` 2)]]

list_n_and_SCLCNF0 = [(n,lgCNF0 n (replicate upBound 0))|n<-[0..upBound]]

-----------------------------------------------------
--  generating with variable size  0
-----------------------------------------------------
-- The memory for storing the computation of lgCNF1
memorySCLCNF1 :: Int -> [Int] -> MemSC
memorySCLCNF1 0 m = LoadSC [lgCNF1 n (reverse m) | n<-[0..]]
memorySCLCNF1 k m = MemSC [memorySCLCNF1 (k-1) (j:m) | j<-[0..]] 

theMemorySCLCNF1 = memorySCLCNF1 (upBound) []

accSCLCNF1 n m = accessSC theMemorySCLCNF1 n m

-- memory of neutrals1

memorySCLNeutrals1 :: Int -> [Int] -> MemSC
memorySCLNeutrals1 0 m = LoadSC [lgNeutrals1 n (reverse m) | n<-[0..]]
memorySCLNeutrals1 k m = MemSC [memorySCLNeutrals1 (k-1) (j:m) | j<-[0..]] 

theMemorySCLNeutrals1 = memorySCLNeutrals1 (upBound) []

accSCLNeutrals1 :: Int -> [Int] -> [SwissCheese]
accSCLNeutrals1 n m = accessSC theMemorySCLNeutrals1 n m

-- Neutrals1
lgNeutrals1 :: Int -> [Int] -> [SwissCheese]
lgNeutrals1 0 m = if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
lgNeutrals1 1 m =  if (head m == 2 && all ((==) 0) (tail m)) -- there is only □0 □0 
              then [AppSC (Box 0) (Box 0)]
              else []
lgNeutrals1 n m =
  foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accSCLNeutrals1 k q)
                                                        (accSCLCNF1 nk r))
                                              (allCombinations m (n-1)))

-- generating linear terms that are abstractions with binding at depth i
lgLCNF1ABSAtD n m i = foldr (++) [] (map (abstract (i-1)) (accSCLCNF1 (n - 2)
                                                      (tail (inc i m) ++ [0])))

-- generating linear terms that are abstractions with binding
lgLCNF1ABSwB :: Int -> [Int] -> [SwissCheese]
lgLCNF1ABSwB n m 
  | head m == 0 = foldr (++) [] [lgLCNF1ABSAtD n m i |i<-[1..(n-1)]]
  | otherwise = []
                
lgCNF1 :: Int -> [Int] -> [SwissCheese]
lgCNF1 0 m = lgNeutrals1 0 m
lgCNF1 n m =  lgNeutrals1 n m ++ lgLCNF1ABSwB n m

listSCLCNF1 = [lgCNF1 n (replicate upBound 0)|n<-[0..upBound]]
listSCLCNF1By2 =  [lgCNF1 (2*k+1) (replicate upBound 0)|k<-[0..(upBound `div` 2)]]

list_n_and_SCLCNF1 = [(n,lgCNF1 n (replicate upBound 0))|n<-[0..upBound]]



--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
