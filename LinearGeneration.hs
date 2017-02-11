-- A program for generating  linear closed λ-terms
-- using natural size
module LinearGeneration where

import NaturalSize
import Affine
import Linear
import SwissCheese
import AffineGeneration

import Data.Array.IArray
import Control.Monad.State
import System.Random

-- =========================================================
--   Generating linear closed terms 
-- =========================================================

------------------------------
-- A naive version
------------------------------

lg' :: Int -> [Int] -> [SwissCheese]
lg' 0 m = if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
lg' n m = let allApp = foldl (++) [] (map (\((q,r),(k,nk))-> appSC (cartesian (lg' k q) (lg' nk r))) (allCombinations m (n-1)))
              allAbs 0 = []
              allAbs i = allAbs (i-1) ++ foldl (++) [] (map (abstract (i-1)) (lg' (n - i - 1) (tail (inc i m))))
          in  allApp ++
              if  head m == 0 then allAbs (n - 1) else []

----------------------------------------
-- An efficient algorithm using a trie
----------------------------------------
           
-- Constants

memoryLG :: Int -> [Int] -> MemSC
memoryLG 0 m = LoadSC [lg n (reverse m) | n<-[0..]]
memoryLG k m = MemSC [memoryLG (k-1) (j:m) | j<-[0..]] 

theMemoryLG = memoryLG (upBound) []

accLG :: Int -> [Int] -> [SwissCheese]
accLG n m = accessSC theMemoryLG n m

-- lg returns all the SwissCheeses for a size n and a tuple m

lg :: Int -> [Int] -> [SwissCheese]
lg 0 m =  if (head m == 1 && all ((==) 0) (tail m)) then [Box 0] else []
lg n m = let allApp = foldr (++) [] (map (\((q,r),(k,nk))-> appSC (cartesian (accLG k q) (accLG nk r))) (allCombinations m (n-1)))
             allAbs 0 = []
             allAbs i = allAbs (i-1) ++ foldr (++) [] (map (abstract (i-1)) (accLG (n - i - 1) (tail (inc i m) ++ [0])))
         in  allApp ++
             if head m == 0 then allAbs (n-1) else []

allClosedLinear = [lg n (replicate upBound 0) | n<-[0..upBound]]

array_lg i = let nbLini = allNbLinear !! i
              in array (1,nbLini) (zip [1..nbLini] (allClosedLinear !! i))
                 :: Array Integer SwissCheese

-- =====================================================
-- Generating random linear terms
-- =====================================================

randomClosedLinearTerm :: Int -> Gen SwissCheese
randomClosedLinearTerm i =
  do randomDouble <- rand
     let randomIndex = round ((fromInteger (allNbLinear !! i)) * randomDouble)
     return ((array_lg i) ! randomIndex)

aClosedLinear :: Int -> Int -> SwissCheese
aClosedLinear sz seed =  evalState (randomClosedLinearTerm sz) (mkStdGen seed)


--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
