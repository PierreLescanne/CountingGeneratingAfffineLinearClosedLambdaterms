-- This module deals with affine closed normal forms in λ-calculus
-- 1. a list of numbers of affine closed normal form fof size n: list_nbACNF
-- 2. a list oif lists of affine closed normal forms of size n; list_ACNF
-- 3. a random generator of affine closed normal form: anAffineCNF sz seed
--                    (where sz is the size and seed a seed for the random generator).

module LinearClosedNormalForm where

import Data.Array.IArray
import Control.Monad.State
import System.Random

import NaturalSize
import TermUnranking
import Affine
import NormalForm
import SwissCheese

-----------------------------------------------------
--  counting
-----------------------------------------------------
-- The memory for storing the computation of amCNF
memoryLCNF :: Int -> [Int] -> Mem
memoryLCNF 0 m = Load [lmCNF n (reverse m) | n<-[0..]]
memoryLCNF k m = Mem [memoryLCNF (k-1) (j:m) | j<-[0..]] 

theMemoryLCNF = memoryLCNF (upBound) []

-- memory of normal forms
accLNF n m = access theMemoryLCNF n m

memoryLNeutrals :: Int -> [Int] -> Mem
memoryLNeutrals 0 m = Load [lmNeutrals n (reverse m) | n<-[0..]]
memoryLNeutrals k m = Mem [memoryLNeutrals (k-1) (j:m) | j<-[0..]] 

theMemoryLNeutrals = memoryLNeutrals (upBound) []

-- memory of neutrals
accLNeutrals :: Int -> [Int] -> Integer
accLNeutrals n m = access theMemoryLNeutrals n m

-- Neutrals
lmNeutrals :: Int -> [Int] -> Integer
lmNeutrals 0 m = iv (head m == 1 && all ((==) 0) (tail m))
lmNeutrals n m = sum (map (\((q,r),(k,nk))->(accLNeutrals k q)*(accLNF nk r)) (allCombinations m (n-1))) 

-- counting affine terms that are abstractions with binding at depth i
lmLCNfABSAtD n m i = (fromIntegral (1 + m!!i))*(accLNF (n - i - 1) ((tail (inc i m)) ++ [0]))

-- counting affine terms that are abstractions with binding
lmLCNfABSwB :: Int -> [Int] -> Integer
lmLCNfABSwB n m 
  | head m == 0 = sum [lmLCNfABSAtD n m i |i<-[1..(n-1)]]
  | otherwise = 0
                
lmCNF :: Int -> [Int] -> Integer

lmCNF 0 m = lmNeutrals 0 m
lmCNF n m =  lmNeutrals n m + lmLCNfABSwB n m

list_nbLCNF = [lmCNF n (replicate upBound 0)|n<-[0..upBound]]

list_n_and_nbLCNF = [(n,lmCNF n (replicate upBound 0))|n<-[0..upBound]]

----------------------------------------
-- generating
----------------------------------------

memoryGLCNF :: Int -> [Int] -> MemSC
memoryGLCNF 0 m = LoadSC [lmGLCNF n (reverse m) | n<-[0..]]
memoryGLCNF k m = MemSC [memoryGLCNF (k-1) (j:m) | j<-[0..]] 

theMemoryGLCNF = memoryGLCNF (upBound) []

accGLCNF :: Int -> [Int] -> [SwissCheese]
accGLCNF n m = accessSC theMemoryGLCNF n m

memoryGLNeutrals :: Int -> [Int] -> MemSC
memoryGLNeutrals 0 m = LoadSC [allAPPNeutral n (reverse m) | n<-[0..]]
memoryGLNeutrals k m = MemSC [memoryGLNeutrals (k-1) (j:m) | j<-[0..]] 

theMemoryGLNeutrals = memoryGLNeutrals (upBound) []

accGLNeutrals :: Int -> [Int] -> [SwissCheese]
accGLNeutrals n m = accessSC theMemoryGLNeutrals n m

allAPPNeutral  :: Int -> [Int] -> [SwissCheese]
allAPPNeutral 0 m = if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
allAPPNeutral n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accGLNeutrals k q)
                                                                   (accGLCNF nk r))
                            (allCombinations m (n-1)))

allABSNFAtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSNFAtD n m i = foldr (++) [] (map (abstract (i-1)) (accGLCNF (n - i - 1)
                                                      (tail (inc i m) ++ [0])))
                  
allABSNFwB  :: Int -> [Int] -> [SwissCheese]
allABSNFwB n m 
  | head m == 0 = foldr (++) [] [allABSNFAtD n m i |i<-[1..(n-1)]]
  | otherwise = []
                
lmGLCNF :: Int -> [Int] -> [SwissCheese]
lmGLCNF 0 m = allAPPNeutral 0 m
lmGLCNF n m = allAPPNeutral n m ++ allABSNFwB n m

list_LCNF = [lmGLCNF n (replicate upBound 0) | n<-[0..upBound]]

array_GLCNF i = let nbLCNFi = list_nbLCNF !! i
              in array (1,nbLCNFi) (zip [1..nbLCNFi] (list_LCNF !! i))
                 :: Array Integer SwissCheese

-- =========================================================
--  Generating random closed linear normal forms
-- =========================================================

type Gen = State StdGen

rand :: Gen Double
rand = do generator <- get
          let (value, newGenerator) = randomR (0,1) generator
          put newGenerator
          return value

randomClosedLinearNormalForm :: Int -> Gen SwissCheese
randomClosedLinearNormalForm i =
  do randomDouble <- rand
     let randomIndex = round ((fromInteger (list_nbLCNF !! i)) * randomDouble)
     return ((array_GLCNF i) ! randomIndex)

aLinearCNF :: Int -> Int -> SwissCheese
aLinearCNF sz seed =  evalState (randomClosedLinearNormalForm sz) (mkStdGen seed)

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
