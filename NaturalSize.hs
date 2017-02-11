module NaturalSize where

import Data.List
-- import Data.Number.CReal
type CReal = Double
-- import Network.HostName

rhoCR :: CReal
rhoCR =0.29559774252208477098099659285153861389897544844661

invRhoCR :: CReal
invRhoCR = 1 / rhoCR

rho :: Double
rho = 0.29559774252208393

-- Iverson symbol
iv b = if b then 1 else 0

----------------
-- Natural size
----------------

--  number of all terms of same natural size
nl :: [Integer]
nl = 0:(zipWith (+) (zipWith (+) [1,1..] nl)
                    [let nli = take n nl in
                     sum $ zipWith (*) nli (reverse nli) | n <-[1..]])

-- for every m (number of indices)
natural :: [[Integer]]
natural = [0,0..] : [[iv (n - 1 < m) + 
                               natural !! (n-1) !! (m+1) + 
                               s n m 
                              | m <- [0..]] | n <- [1..]]
  where s n m  = let ti = [natural !! i !! m | i <- [0..(n-1)]] in 
          sum $ zipWith (*) ti (reverse ti)

-- the above double table as a function
-- of number of indices and size
-- to numbers of terms
natSizeNb :: Int -> Int -> Integer
natSizeNb m n = natural !! n !! m

---------------------
-- Head Normal Forms
---------------------

h :: [Integer]
h = 0: zipWith (+) (tail neutral) h

neutral :: [Integer]
neutral = 0 : zipWith (+) [1,1..] 
                    [let nli = take n nl 
                         ki = take n neutral in
                    sum $ zipWith (*) ki (reverse nli) | n <-[1..]]

------------------------------------
-- Natural size in which |0| = 0
------------------------------------

ll :: [Integer]
ll = 1:(zipWith (+) (zipWith (+) [1,1..] ll)
                    [let lli = take n ll in
                     sum $ zipWith (*) lli (reverse lli) | n <-[1..]])

----------------------
-- Sapounakis formula
----------------------

sap :: Int -> Int
sap n = sum [t k | k <- [0..(n-1) `div` 2]]
          where t k = (-1)^k * (binomial!!(n-k)!!k) * (binomial!!(2*n-3*k)!!(n-2*k-1))`div` (n-k)
-------------------
-- Asymptotics
-------------------
constant :: CReal
constant = 0.6067673777880385515093941547

-- natural size approximation
formula :: Int -> CReal
formula n = invRhoCR ** cRn  * constant / ((sqrt cRn) * cRn)
    where cRn = fromIntegral n

-- tests of the accuracy of the approximation 
nl_versus_formula :: [(Int,Integer,String)]
nl_versus_formula = map (\n->(n,nl!!n,show (formula n))) [10,20..]

-- head forms
constant_hd_forms :: CReal
constant_hd_forms = 0.2546259118367629463900574771

formulaHdNf :: Int -> CReal
formulaHdNf n = invRhoCR ** cRn  * constant_hd_forms / ((sqrt cRn) * cRn)
    where cRn = fromIntegral n

-- tests  of the accuracy of the approximation for head normal forms
h_versus_formula :: [(Int,Integer,String)]
h_versus_formula = map (\n->(n,h!!n,show (formulaHdNf n))) [10,20..]


-------------------
-- Binomial 
-------------------

binomial :: [[Int]]
binomial = (1:[0,0..]) : map (\s->1 : zipWith (+) s (tail s)) binomial
