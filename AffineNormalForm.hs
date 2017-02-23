-- This module deals with affine closed normal forms in λ-calculus
-- 1. a list of numbers of affine closed normal form fof size n: list_nbACNF
-- 2. a list oif lists of affine closed normal forms of size n; list_ACNF
-- 3. a random generator of affine closed normal form: anAffineCNF sz seed
--                    (where sz is the size and seed a seed for the random generator).

module AffineClosedNormalForm where

import Data.Array.IArray
import Control.Monad.State
import System.Random

import NaturalSize
import TermUnranking
import Affine
import NormalForm
import SwissCheese

-----------------------------------------------------
-- counting
-----------------------------------------------------
-- The memory for storing the computation of amCNF
memoryACNF :: Int -> [Int] -> Mem
memoryACNF 0 m = Load [amCNF n (reverse m) | n<-[0..]]
memoryACNF k m = Mem [memoryACNF (k-1) (j:m) | j<-[0..]] 

theMemoryACNF = memoryACNF (upBound) []

-- memory of normal forms
accACNF n m = access theMemoryACNF n m

memoryANeutrals :: Int -> [Int] -> Mem
memoryANeutrals 0 m = Load [amNeutrals n (reverse m) | n<-[0..]]
memoryANeutrals k m = Mem [memoryANeutrals (k-1) (j:m) | j<-[0..]] 

theMemoryANeutrals = memoryANeutrals (upBound) []

-- memory of neutrals
accNeutrals :: Int -> [Int] -> Integer
accNeutrals n m = access theMemoryANeutrals n m

-- Neutrals
amNeutrals :: Int -> [Int] -> Integer
amNeutrals 0 m = iv (head m == 1 && all ((==) 0) (tail m))
amNeutrals n m = sum (map (\((q,r),(k,nk))->(accNeutrals k q)*(accACNF nk r)) (allCombinations m (n-1))) 

-- counting affine terms that are abstractions with binding at depth i
amACNfABSAtD n m i = (fromIntegral (1 + m!!i))*(accACNF (n - i - 1) ((tail (inc i m)) ++ [0]))

-- counting affine terms that are abstractions with binding
amACNfABSwB :: Int -> [Int] -> Integer
amACNfABSwB n m 
  | head m == 0 = sum [amACNfABSAtD n m i |i<-[1..(n-1)]]
  | otherwise = 0
                
-- counting affine terms that are abstractions with no binding
amACNfABSnB :: Int -> [Int] -> Integer
amACNfABSnB n m 
    | head m == 0 = (accACNF (n-1) (tail m ++ [0]))
    | otherwise = 0

amCNF :: Int -> [Int] -> Integer
amCNF 0 m = amNeutrals 0 m
amCNF n m = amNeutrals n m + amACNfABSwB n m + amACNfABSnB n m 

list_nbACNF = [amCNF n (replicate upBound 0)|n<-[0..upBound]]

list_n_and_nbACNF = zip [0..upBound] list_nbACNF

----------------------------------------
-- generating
----------------------------------------

memoryGACNF :: Int -> [Int] -> MemSC
memoryGACNF 0 m = LoadSC [amGACNF n (reverse m) | n<-[0..]]
memoryGACNF k m = MemSC [memoryGACNF (k-1) (j:m) | j<-[0..]] 

theMemoryGACNF = memoryGACNF (upBound) []

accGACNF :: Int -> [Int] -> [SwissCheese]
accGACNF n m = accessSC theMemoryGACNF n m

memoryGNeutrals :: Int -> [Int] -> MemSC
memoryGNeutrals 0 m = LoadSC [allAPPNeutral n (reverse m) | n<-[0..]]
memoryGNeutrals k m = MemSC [memoryGNeutrals (k-1) (j:m) | j<-[0..]] 

theMemoryGNeutrals = memoryGNeutrals (upBound) []

accGNeutrals :: Int -> [Int] -> [SwissCheese]
accGNeutrals n m = accessSC theMemoryGNeutrals n m

allAPPNeutral  :: Int -> [Int] -> [SwissCheese]
allAPPNeutral 0 m = if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
allAPPNeutral n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accGNeutrals k q)
                                                                   (accGACNF nk r))
                            (allCombinations m (n-1)))

allABSNFAtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSNFAtD n m i = foldr (++) [] (map (abstract (i-1)) (accGACNF (n - i - 1)
                                                      (tail (inc i m) ++ [0])))
                  
allABSNFwB  :: Int -> [Int] -> [SwissCheese]
allABSNFwB n m 
  | head m == 0 = foldr (++) [] [allABSNFAtD n m i |i<-[1..(n-1)]]
  | otherwise = []

allABSNFnB  :: Int -> [Int] -> [SwissCheese]
allABSNFnB n m
  | head m == 0 = map (AbsSC . raise) (accGACNF (n-1) (tail m ++ [0]))
  | otherwise = []
                
amGACNF :: Int -> [Int] -> [SwissCheese]
amGACNF 0 m = allAPPNeutral 0 m
amGACNF n m = allAPPNeutral n m ++ allABSNFwB n m ++ allABSNFnB n m 

list_ACNF = [amGACNF n (replicate upBound 0) | n<-[0..upBound]]

arrayACNF i = let nbACNFi = list_nbACNF !! i
              in array (1,nbACNFi) (zip [1..nbACNFi] (list_ACNF !! i))
                 :: Array Integer SwissCheese

-- =========================================================
--  Generating random affine closed normal forms
-- =========================================================

type Gen = State StdGen

rand :: Gen Double
rand = do generator <- get
          let (value, newGenerator) = randomR (0,1) generator
          put newGenerator
          return value

randomClosedAffineNormalForm :: Int -> Gen SwissCheese
randomClosedAffineNormalForm i =
  do randomDouble <- rand
     let randomIndex = round ((fromInteger (list_nbACNF !! i)) * randomDouble)
     return ((arrayACNF i) ! randomIndex)

anAffineCNF :: Int -> Int -> SwissCheese
anAffineCNF sz seed =  evalState (randomClosedAffineNormalForm sz) (mkStdGen seed)
     -- limit 23 

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
