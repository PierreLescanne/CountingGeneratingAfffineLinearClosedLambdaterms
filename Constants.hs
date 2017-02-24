module Constants where

upBound = 30  -- the bound for looking for terms
m0 = replicate upBound (0::Int)  -- a tuple made of 0's
m1 = 1: (replicate (upBound -1) (0::Int)) -- a tuple made of a 1 followed by 0's

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
