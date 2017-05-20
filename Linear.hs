-- A program for counting linear closed λ-terms using natural size
module Linear where

import Constants

import NaturalSize
import TermUnranking
import Affine
import SwissCheese

---------------------------
-- Linearity
---------------------------
-- iL returns a list of Int if all the binders of the term binds
-- one and only one de Bruijn index. 
iL :: Term -> Maybe [Int]
iL (Index n) = Just (replicate n 0 ++ [1])
iL (Abs t) = let mn = iL t
             in case mn of
                Nothing -> Nothing
                Just n -> if null n || head n /= 1
                          then Nothing
                          else Just (tail n)
iL (App t1 t2) =  let mn1 = iL t1
                      mn2 = iL t2
                      zipPlus l1 [] = l1
                      zipPlus [] l2 = l2
                      zipPlus (a1:l1) (a2:l2) = (a1+a2):(zipPlus l1 l2)
                  in case mn1 of
                      Nothing -> Nothing
                      Just n1 -> case mn2 of
                        Nothing -> Nothing
                        Just n2 -> Just (zipPlus n1 n2)
                        
-- is linear in the sense that it is a closed term and
-- all the binders bound one and only one index. 
isLinear :: Term -> Bool 
isLinear t = let mn = iL t
             in case mn of
                Nothing -> False
                Just _ -> True


-- iLSC returns a list of Int if all the binders of the term binds
-- one and only one de Bruijn index. 
iLSC :: SwissCheese -> Maybe [Int]
iLSC (IndexSC n) = Just ((replicate (deBruijn2Int n) 0) ++ [1])
iLSC (AbsSC t) = let mn = iLSC t
             in case mn of
                Nothing -> Nothing
                Just n -> if null n || head n /= 1
                          then Nothing
                          else Just (tail n)
iLSC (AppSC t1 t2) =  let mn1 = iLSC t1
                          mn2 = iLSC t2
                          zipPlus l1 [] = l1
                          zipPlus [] l2 = l2
                          zipPlus (a1:l1) (a2:l2) = (a1+a2):(zipPlus l1 l2)
                      in case mn1 of
                      Nothing -> Nothing
                      Just n1 -> case mn2 of
                        Nothing -> Nothing
                        Just n2 -> Just (zipPlus n1 n2)
iLSC (Box _) = Nothing                       

-- is linear in the sense that it is a closed term and
-- all the binders bound one and only one index. 
isLinearSC :: SwissCheese -> Bool 
isLinearSC t = let mn = iLSC t
               in case mn of
                 Nothing -> False
                 Just _ -> True

----------------------------------------
-- An efficient algorithm using a trie
----------------------------------------

-- Constants

memoryL :: Int -> [Int] -> Mem
memoryL 0 m = Load [nbLinear n (reverse m) | n<-[0..]]
memoryL k m = Mem [memoryL (k-1) (j:m) | j<-[0..]] 

theMemoryL = memoryL(upBound) []

accL :: Int -> [Int] -> Integer
accL n m = access theMemoryL n m

-- 
nbLinear :: Int -> [Int] -> Integer
nbLinear 0 m = iv (head m == 1 && all ((==) 0) (tail m))
nbLinear n m = let sumApp = sum (map (\((q,r),(k,nk))->(accL k q)*(accL nk r)) (allCombinations m (n-1)))
                   sumAbs 0 = 0
                   sumAbs i = sumAbs (i-1) +
                              (fromIntegral(1 + m!!i))*(accL (n-i-1) (tail (inc i m)++[0]))
               in  sumApp +
                   if head m == 0
                   then sumAbs (n-1)
                   else 0

allNbLinear = [nbLinear n (replicate upBound 0) | n<-[0..upBound]]

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
