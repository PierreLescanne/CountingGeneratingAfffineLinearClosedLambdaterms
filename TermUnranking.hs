module TermUnranking where
import NaturalSize
import System.Random

-- generating or unranking

data Term = Index Int
          | Abs Term
          | App Term Term

instance Show Term where 
    show (Index n) = show n
    show (Abs t @ (App _ _)) = "λ" ++ show t
    show (Abs t @ (Index _)) = "λ" ++ show t
    show (Abs t @ (Abs _)) = "λ" ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

-- Tromp size
trompSize :: Term -> Int
trompSize (Index n) = n + 1
trompSize (Abs t) = trompSize t + 2
trompSize (App t1 t2) = trompSize t1 + trompSize t2 + 2

-- natural size
naturalSize :: Term -> Int
naturalSize (Index n) = n + 1
naturalSize (Abs t) = naturalSize t + 1
naturalSize (App t1 t2) = naturalSize t1 + naturalSize t2 + 1

-- Unranking a term with at most `m` indices, of size `n`, from rank `k`
unrankL :: Int -> Int -> Integer -> Term
unrankL m n k
  | m >= n && k == (natSizeNb m n) = Index (n - 1)
  | k <= (natSizeNb (m+1) (n-1)) = Abs (unrankL (m+1) (n-1) k)
  | otherwise = unrankApp (n-1) 0 (k - natSizeNb (m+1) (n-1))
    where unrankApp n j r
            | r <=  tmjtmnj  = let (dv,rm) = (r-1) `divMod` tmnj
                               in App (unrankL m j (dv+1)) (unrankL  m (n-j) (rm+1))
            | otherwise = unrankApp n (j + 1) (r -tmjtmnj) 
            where tmnj = natSizeNb m (n-j) 
                  tmjtmnj = (natSizeNb m j) * tmnj 

-- Unranking a term from a size and and a number
unrankLoo :: Int -> Integer -> Term
unrankLoo n k
  | k == (nl !! n) = Index (n - 1)
  | k <= (nl !! (n-1)) = Abs (unrankLoo (n-1) k)                     
  | otherwise = unrankApp (n-1) 0 (k - nl !! (n-1))
    where unrankApp n j r 
            | r <= tjnj = let (dv,rm) = (r-1) `divMod` tnj
                          in App (unrankLoo j (dv+1)) (unrankLoo (n-j) (rm+1))
            | otherwise = unrankApp n (j+1) (r-tjnj)
            where tnj = nl!!(n-j)
                  tjnj = (nl!!j) * tnj

-- List of of all the closed terms of size `size`
list_closed_terms :: Int -> [Term]
list_closed_terms size = map (unrankL 0 size) [1.. (natSizeNb 0 size)]

-- List of ter terms of size `size` with at most `m` indices
list_terms :: Int -> Int -> [Term]
list_terms size m =  map (unrankL m size) [1..(natSizeNb 0 size)]

-- List of of all terms of size `size`
list_all_terms :: Int -> [Term]
list_all_terms size = map (unrankLoo size) [1.. (nl !! size)]

