module LinearGenerationSize0or1 where

import NaturalSize
import TermUnranking
import Affine
import SwissCheese
import LinearSize0or1

-----------------------------------------------------
--  generating with variable size 0
-----------------------------------------------------
-- Constants

memoryLG0 :: Int -> [Int] -> MemSC
memoryLG0 0 m = LoadSC [lg0 n (reverse m) | n<-[0..]]
memoryLG0 k m = MemSC [memoryLG0 (k-1) (j:m) | j<-[0..]] 

theMemoryLG0 = memoryLG0 upBound []

accLG0 :: Int -> [Int] -> [SwissCheese]
accLG0 n m = accessSC theMemoryLG0 n m

-- generating linear terms that are applications
allAPPG0  :: Int -> [Int] -> [SwissCheese]
allAPPG0 n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accLG0 k q)
                                                                     (accLG0 nk r))
                            (allCombinations m (n-1)))

-- generating linear terms that are abstractions with binding at depth i
allABSG0AtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSG0AtD n m i = foldr (++) [] (map (abstract (i-1)) (accLG0 (n - 1)
                                                        (tail (inc i m) ++ [0])))
                  
-- generating linear terms that are abstractions with binding
allABSG0wB  :: Int -> [Int] -> [SwissCheese]
allABSG0wB n m 
  | head m == 0 = foldr (++) [] [allABSG0AtD n m i |i<-[1..n]]
  | otherwise = []
                
lg0 :: Int -> [Int] -> [SwissCheese]
lg0 0 m =  if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
lg0 n m = allAPPG0 n m ++ allABSG0wB n m

list_lg0 = [lg0 n (replicate upBound 0) | n<-[0..upBound]]

-----------------------------------------------------
--  generating with variable size  1
-----------------------------------------------------
-- Constants

memoryLG1 :: Int -> [Int] -> MemSC
memoryLG1 0 m = LoadSC [lg1 n (reverse m) | n<-[0..]]
memoryLG1 k m = MemSC [memoryLG1 (k-1) (j:m) | j<-[0..]] 

theMemoryLG1 = memoryLG1 upBound []

accLG1 :: Int -> [Int] -> [SwissCheese]
accLG1 n m = accessSC theMemoryLG1 n m

-- generating linear terms that are applications
allAPPG1  :: Int -> [Int] -> [SwissCheese]
allAPPG1 n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accLG1 k q)
                                                                     (accLG1 nk r))
                            (allCombinations m (n-1)))

-- generating linear terms that are abstractions with binding at depth i
allABSG1AtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSG1AtD n m i = foldr (++) [] (map (abstract (i-1)) (accLG1 (n - 2)
                                                               (tail (inc i m) ++ [0])))
                  
-- generating linear terms that are abstractions with binding
allABSG1wB  :: Int -> [Int] -> [SwissCheese]
allABSG1wB n m 
  | head m == 0 = foldr (++) [] [allABSG1AtD n m i |i<-[1..(n-1)]]
  | otherwise = []
                
lg1 :: Int -> [Int] -> [SwissCheese]
lg1 0 m =  if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
lg1 n m = allAPPG1 n m ++ allABSG1wB n m

list_lg1 = [lg1 n (replicate upBound 0) | n<-[0..upBound]]

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
