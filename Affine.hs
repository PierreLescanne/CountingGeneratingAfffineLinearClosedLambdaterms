-- A program for counting affine closed λ-terms using natural size
module Affine where

import NaturalSize
import TermUnranking

-- For example: cartesian [1,2] [3,4] = [(1,3),(1,4),(2,3),(2,4)]
cartesian :: [a] -> [b] -> [(a,b)]
cartesian [] _ = []
cartesian _ [] = []
cartesian (a1:l1) l2 = (map ((,) a1) l2) ++ cartesian l1 l2

-------------------------------------------
-- Operations on tuples 
-------------------------------------------
inc :: Int -> [Int] -> [Int]
inc _ [] = []
inc 0 (k:m) = (k+1):m
inc i (k:m) = k:(inc (i-1) m)
                         
-- all the ways to split an integer
splitInt i = zip [0..i] (reverse [0..i])

glue :: [(a, b)] -> [([a], [b])] -> [([a], [b])]
glue l1 l2 = map (\((a,b),(c,d)) -> (a:c,b:d)) (cartesian l1 l2)

-- split an uple in sums of tuples
split :: [Int] -> [([Int],[Int])]
split [] = []
split [n] =  map (\(a,b)->([a],[b])) (splitInt n)
split (n:l) = glue (splitInt n) (split l)

-- uple have the same size, so they are filled up to upBound.
splitFill m = let fill l = l ++ (replicate (upBound - length l) 0)
              in map (\(l1,l2)->(fill l1,fill l2)) (split m)

-- iA returns a list of Int if all the binders of the term binds
-- at most one de Bruijn index. 
iA :: Term -> Maybe [Int]
iA (Index n) = Just ((replicate n 0) ++ [1])
iA (Abs t) = let mn = iA t
             in case mn of
                Nothing -> Nothing
                Just n -> if null n then Just ([])
                          else if head n > 1 then Nothing
                               else Just (tail n)
iA (App t1 t2) =  let mn1 = iA t1
                      mn2 = iA t2
                      zipPlus l1 [] = l1
                      zipPlus [] l2 = l2
                      zipPlus (a1:l1) (a2:l2) = (a1+a2):(zipPlus l1 l2)
                  in case mn1 of
                     Nothing -> Nothing
                     Just n1 -> case mn2 of
                       Nothing -> Nothing
                       Just n2 -> Just (zipPlus n1 n2)

-- is affine in the sense that all the bound indices are bound at most one. 
isAffine :: Term -> Bool 
isAffine t = let mn = iA t
             in case mn of
                Nothing -> False
                Just _ -> True

-- ============================================================
-- Counting affine closed terms 
-- ============================================================

-- The parameters for the first component of the sum (applications)
allCombinations :: [Int] -> Int -> [(([Int],[Int]),(Int,Int))]
allCombinations m n = cartesian (splitFill m) (splitInt n)

-- treelike memory 
data Mem = Mem [Mem]
         | Load [Integer]

instance Show Mem where
  show h = let showT 0 _ = "?"
               showT i (Mem c) = "{" ++ showT (i-1) (c!!0) ++
                                   "," ++ showT (i-1) (c!!1) ++
                                   "," ++ showT (i-1) (c!!2) ++ "}\n"
               showT _ (Load l) = show (take 4 l)
           in showT 5 h

access :: Mem -> Int -> [Int] -> Integer
access (Load l) n []  = l !! n
access (Mem listM) n (k:m) = access (listM !! k) n m

-- Constants
upBound = 40
m0 = replicate upBound (0::Int)
m1 = 1: (replicate (upBound -1) (0::Int))

-----------------------------------------------------
-- An efficient  version counting separately classes
-----------------------------------------------------
-- The memory for storing the computation of am
memory :: Int -> [Int] -> Mem
memory 0 m = Load [am n (reverse m) | n<-[0..]]
memory k m = Mem [memory (k-1) (j:m) | j<-[0..]] 

theMemory = memory (upBound) []

acc :: Int -> [Int] -> Integer
acc n m = access theMemory n m

-- counting affine terms that are applications
amAPP :: Int -> [Int] -> Integer
amAPP n m = sum (map (\((q,r),(k,nk))->(acc k q)*(acc nk r)) (allCombinations m (n-1)))

-- counting affine terms that are abstractions with binding at depth i
amABSAtD n m i = (fromIntegral (1 + m!!i))*(acc (n - i - 1) ((tail (inc i m)) ++ [0]))

-- counting affine terms that are abstractions with binding
amABSwB :: Int -> [Int] -> Integer
amABSwB n m 
  | head m == 0 = sum [amABSAtD n m i |i<-[1..(n-1)]]
  | otherwise = 0
                
-- counting affine terms that are abstactions with no binding
amABSnB :: Int -> [Int] -> Integer
amABSnB n m 
    | head m == 0 = (acc (n-1) (tail m ++ [0]))
    | otherwise = 0

am :: Int -> [Int] -> Integer
am 0 m = iv (head m == 1 && all ((==) 0) (tail m))
am n m =  amAPP n m + amABSwB n m + amABSnB n m 

list_am = [am n (replicate upBound 0)|n<-[0..upBound]]

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
