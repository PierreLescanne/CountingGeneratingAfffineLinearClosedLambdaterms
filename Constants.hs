module Constants where

upBound = 30  -- the bound for looking for terms
m0 = replicate upBound (0::Int)  -- a tuple made of 0's
m1 = 1: (replicate (upBound -1) (0::Int)) -- a tuple made of a 1 followed by 0's

-- For example: cartesian [1,2] [3,4] = [(1,3),(1,4),(2,3),(2,4)]
cartesian :: [a] -> [b] -> [(a,b)]
cartesian [] _ = []
cartesian _ [] = []
cartesian (a1:l1) l2 = (map ((,) a1) l2) ++ cartesian l1 l2

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
