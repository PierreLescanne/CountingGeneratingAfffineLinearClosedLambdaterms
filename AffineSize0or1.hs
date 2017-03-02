module AffineSize0or1 where

import Constants

import NaturalSize
import TermUnranking
import Affine
import Linear
import SwissCheese

-- ============================================================
--         COUNTING AFFINE TERMS WITH VARIABLE SIZE 0
-- ============================================================

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
am1ABSAtD n m i = (fromIntegral(1 + m!!i))*(accA1 (n-2) (tail (inc i m)++[0]))

-- counting affine terms that are abstractions with binding
am1ABSwB :: Int -> [Int] -> Integer
am1ABSwB n m
  | head m == 0 = sum [am1ABSAtD n m i |i<-[1..(n-1)]]
  | otherwise = 0
 
-- counting affine terms that are abstractions with no binding
am1ABSnB :: Int -> [Int] -> Integer
am1ABSnB n m 
    | head m == 0 = (accA1 (n-1) (tail m ++ [0]))
    | otherwise = 0

am1 :: Int -> [Int] -> Integer
am1 0 m = iv (head m == 1 && all ((==) 0) (tail m)) -- there is only □0
am1 n m = am1App n m + am1ABSwB n m + am1ABSnB n m

nbClosedAffineSize1 = [am1 n (replicate upBound 0) | n<-[0..upBound]]
-- [0,0,1,2,3,9,30,81,242,838,2799,9365,33616,122937,449698,1696724,6558855,25559806,101294687,409363758,
-- versus 0, 0, 1, 2, 3, 9, 30, 81, 225, 702, 2187, 6561, 19602, 59049, 177633, 532170, 1594323, . . .
-- of Grygiel et al "How big is BCI fragmet of BCI logic"

-- ===================================
--     GENERATING AFINE CLOSED TERMS 
-- ===================================

-----------------------------------------------------
--  generating with variable size 0
-----------------------------------------------------
-- Constants

memoryAG0 :: Int -> [Int] -> MemSC
memoryAG0 0 m = LoadSC [ag0 n (reverse m) | n<-[0..]]
memoryAG0 k m = MemSC [memoryAG0 (k-1) (j:m) | j<-[0..]] 

theMemoryAG0 = memoryAG0 upBound []

accAG0 :: Int -> [Int] -> [SwissCheese]
accAG0 n m = accessSC theMemoryAG0 n m

-- generating linear terms that are applications
allAPPAG0  :: Int -> [Int] -> [SwissCheese]
allAPPAG0 n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accAG0 k q)
                                                                     (accAG0 nk r))
                            (allCombinations m (n-1)))

-- generating linear terms that are abstractions with binding at depth i
allABSAG0AtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSAG0AtD n m i = foldr (++) [] (map (abstract (i-1)) (accAG0 (n - 1)
                                                        (tail (inc i m) ++ [0])))
                  
-- generating linear terms that are abstractions with binding
allABSAG0wB  :: Int -> [Int] -> [SwissCheese]
allABSAG0wB n m 
  | head m == 0 = foldr (++) [] [allABSAG0AtD n m i |i<-[1..n]]
  | otherwise = []
                
-- generating linear terms that are abstractions without binding
allABSAG0nB  :: Int -> [Int] -> [SwissCheese]
allABSAG0nB n m
  | head m == 0 = map (AbsSC . raise) (accAG0 (n-1) (tail m ++ [0]))
  | otherwise = []


ag0 :: Int -> [Int] -> [SwissCheese]
ag0 0 m =  if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
ag0 n m = allAPPAG0 n m ++ allABSAG0wB n m ++ allABSAG0nB n m

list_ag0 = [ag0 n (replicate upBound 0) | n<-[0..upBound]]

-----------------------------------------------------
--  generating with variable size 1
-----------------------------------------------------
-- Constants
-- CHECK
memoryAG1 :: Int -> [Int] -> MemSC
memoryAG1 0 m = LoadSC [ag1 n (reverse m) | n<-[0..]]
memoryAG1 k m = MemSC [memoryAG1 (k-1) (j:m) | j<-[0..]] 

theMemoryAG1 = memoryAG1 upBound []

accAG1 :: Int -> [Int] -> [SwissCheese]
accAG1 n m = accessSC theMemoryAG1 n m

-- generating linear terms that are applications
allAPPAG1  :: Int -> [Int] -> [SwissCheese]
allAPPAG1 n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accAG1 k q)
                                                                     (accAG1 nk r))
                            (allCombinations m (n-1)))

-- generating linear terms that are abstractions with binding at depth i
allABSAG1AtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSAG1AtD n m i = foldr (++) [] (map (abstract (i-1)) (accAG1 (n - 2)
                                                        (tail (inc i m) ++ [0])))
                  
-- generating linear terms that are abstractions with binding
allABSAG1wB  :: Int -> [Int] -> [SwissCheese]
allABSAG1wB n m 
  | head m == 0 = foldr (++) [] [allABSAG1AtD n m i |i<-[1..(n-1)]]
  | otherwise = []
                
-- generating linear terms that are abstractions without binding
allABSAG1nB  :: Int -> [Int] -> [SwissCheese]
allABSAG1nB n m
  | head m == 0 = map (AbsSC . raise) (accAG1 (n-1) (tail m ++ [0]))
  | otherwise = []


ag1 :: Int -> [Int] -> [SwissCheese]
ag1 0 m =  if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
ag1 n m = allAPPAG1 n m ++ allABSAG1wB n m ++ allABSAG1nB n m

list_ag1 = [ag1 n (replicate upBound 0) | n<-[0..upBound]]

-- ==================================================
--     COUNTING CLOSED TERMS WITH VARIABLE SIZE 1
-- ==================================================
-- count first the number of closed terms with variable size 1

mem :: [[Integer]]
mem = [[t m n | n <- [0..]]| m<-[0..]]

nbAppT m n = let tjn1 = take n (mem !! m)
             in sum (zipWith (*) tjn1 (reverse tjn1))

nbAbsT m n =  mem!!(m+1)!!(n-1)

t:: Int -> Int -> Integer
t _ 0 = 0
t m 1 = fromIntegral m
t m n =  nbAppT m n + nbAbsT m n
          
nbClosed = [t 0 n | n<-[0..]]

-- ==================================================
--     GENERATING CLOSED TERMS WITH VARIABLE SIZE 1
-- ==================================================
-- generate closed terùs with variables terms with variable size 1

memg :: [[[SwissCheese]]]
memg = [[gt m n | n <- [0..]]| m<-[0..]]

nbAppGT :: Int -> Int -> [SwissCheese]
nbAppGT m n = let tjn1 = take n (memg !! m)
                  appAll l1 l2 = appSC (cartesian  l1 l2)
              in foldl (++) [] (zipWith appAll tjn1 (reverse tjn1))

nbAbsGT m n =  map AbsSC (memg!!(m+1)!!(n-1))

deBruijnUntil 0 = []
deBruijnUntil m = O:(map S (deBruijnUntil (m-1)))

gt:: Int -> Int -> [SwissCheese]
gt _ 0 = []
gt m 1 = map IndexSC (deBruijnUntil m)
gt m n =  nbAppGT m n ++ nbAbsGT m n
  
allClosed = [gt 0 n | n<-[0..]]

-- ===================================================================
--  COUNTING AFFINE CLOSED TERMS BY GENERRATION FILTERING AND COUNTING
-- ===================================================================

sC2Term (IndexSC d) = Index (deBruijn2Int d)
sC2Term (AbsSC sc) = Abs (sC2Term sc)
sC2Term (AppSC sc1 sc2) = App (sC2Term sc1) (sC2Term sc2) 

isAffinSC sc = isAffine (sC2Term sc)

countAffine = map length (map (filter isAffinSC) allClosed)

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
