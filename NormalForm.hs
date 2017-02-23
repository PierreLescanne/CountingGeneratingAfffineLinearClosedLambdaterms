-- This modules deals with counting and generating
-- normal forms for the natural size
module NormalForm where

import NaturalSize
import System.Random
import TermUnranking
import Affine
import Linear

-- nb of normal forms
nf :: Int -> Int
nf 0 = 0
nf n = ne n + nf (n-1)

-- nb of neutral normal forms
ne 0 = 0 
ne n = 1 + sum[nf i * ne (n-1-i) | i <-[0..(n-1)]]

list_nf :: [Int]
list_nf = [nf n | n<-[0..]]

list_ne :: [Int]
list_ne = [ne n | n<-[0..]]

-- more efficient
list_nf' :: [Integer]
list_nf' = [nf' n | n<-[0..]]

list_ne' :: [Integer]
list_ne' = [ne' n | n<-[0..]]

nf' 0 = 0
nf' n = ne' n + list_nf' !! (n-1)

ne' 0 = 0
ne' n = 1 + sum(let nfi = take n list_nf'
                    nei = take n list_ne'
                in zipWith (*) nfi (reverse nei))
-- ===========
-- generation
-- ===========

closedness :: Term -> Int
closedness (Index n) = n+1
closedness (Abs t) = closedness t  - 1
closedness (App t1 t2) = max (closedness t1) (closedness t2)

isClosed t = closedness t <=0

app :: [(Term,Term)] -> [Term]
app l = map (\(t1,t2) -> App t1 t2) l

normalForms :: Int -> [Term]
normalForms 0 = []
normalForms n = neutrals n ++ map Abs (list_NormalForms !! (n-1))
  
neutrals :: Int -> [Term]
neutrals 0 = []
neutrals n = Index (n-1) :
               foldl (++) [] (map app [cartesian (list_Neutrals !! i)  (list_NormalForms !! (n-i-1)) |i<-[0..n-1]])

list_NormalForms :: [[Term]]
list_NormalForms = [normalForms n | n<-[0..]]

list_Neutrals = [neutrals n | n<-[0..]]

-- closed affine normal forms
closedAffineNF n = filter isAffine (filter isClosed  (list_NormalForms !! n))

-- number of closed affine normal forms
listNbAffineClosedNF = [length$closedAffineNF n | n<-[0..]]

-- closed linear normal forms
closedLinearNF n = filter isLinear (filter isClosed  (list_NormalForms !! n))

-- number of closed linear normal forms
allNbLinearClosedNF = [length$closedLinearNF n | n<-[0..]]

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
