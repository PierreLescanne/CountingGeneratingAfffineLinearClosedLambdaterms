-- This is a program for generating affine and linear closed λ-terms
-- using natural size
module AffineGeneration where

import Constants

import NaturalSize
import Affine
import SwissCheese

import Data.Array.IArray
import Control.Monad.State
import System.Random
                    
-- ============================================================
-- Generating affine terms
-- ============================================================

memoryAG :: Int -> [Int] -> MemSC
memoryAG 0 m = LoadSC [ag n (reverse m) | n<-[0..]]
memoryAG k m = MemSC [memoryAG (k-1) (j:m) | j<-[0..]] 

theMemoryAG = memoryAG (upBound) []

accAG :: Int -> [Int] -> [SwissCheese]
accAG n m = accessSC theMemoryAG n m

allAPP  :: Int -> [Int] -> [SwissCheese]
allAPP n m = foldr (++) [] (map (\((q,r),(k,nk))-> appSC$cartesian (accAG k q)
                                                                   (accAG nk r))
                            (allCombinations m (n-1)))

allABSAtD :: Int -> [Int] -> Int -> [SwissCheese]
allABSAtD n m i = foldr (++) [] (map (abstract (i-1)) (accAG (n - i - 1)
                                                      (tail (inc i m) ++ [0])))
                  
allABSwB  :: Int -> [Int] -> [SwissCheese]
allABSwB n m 
  | head m == 0 = foldr (++) [] [allABSAtD n m i |i<-[1..(n-1)]]
  | otherwise = []

allABSnB  :: Int -> [Int] -> [SwissCheese]
allABSnB n m
  | head m == 0 = map (AbsSC . raise) (accAG (n-1) (tail m ++ [0]))
  | otherwise = []
                
ag :: Int -> [Int] -> [SwissCheese]
ag 0 m =  if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
ag n m = allAPP n m ++ allABSwB n m ++ allABSnB n m 

list_ag = [ag n (replicate upBound 0) | n<-[0..upBound]]

array_ag i = let nbAffi = list_nbAffine !! i
              in array (1,nbAffi) (zip [1..nbAffi] (list_ag !! i))
                 :: Array Integer SwissCheese

-- =====================================================
-- Generating random affine terms
-- =====================================================

type Gen = State StdGen

rand :: Gen Double
rand = do generator <- get
          let (value, newGenerator) = randomR (0,1) generator
          put newGenerator
          return value

randomClosedAffineTerm :: Int -> Gen SwissCheese
randomClosedAffineTerm i =
  do randomDouble <- rand
     let randomIndex = round ((fromInteger (list_nbAffine !! i)) * randomDouble)
     return ((array_ag i) ! randomIndex)

aClosedAffine :: Int -> Int -> SwissCheese
aClosedAffine sz seed =  evalState (randomClosedAffineTerm sz) (mkStdGen seed)
-- limit to compute 19

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
