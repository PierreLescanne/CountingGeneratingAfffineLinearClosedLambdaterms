module SwissCheese where

data SwissCheese = IndexSC DeBruijn
                 | AbsSC SwissCheese
                 | AppSC SwissCheese SwissCheese
                 | Box Int

data DeBruijn = O | S DeBruijn

instance Show SwissCheese where 
  show (IndexSC n) = show n
  show (Box n) = "□" ++ show n
  show (AbsSC t @ (AppSC _ _)) = "λ" ++ show t
  show (AbsSC t @ (IndexSC _)) = "λ" ++ show t
  show (AbsSC t @ (Box _)) = "λ" ++ show t
  show (AbsSC t @ (AbsSC _)) = "λ" ++ show t
  show (AppSC t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

instance Show DeBruijn where
  show d = show (deBruijn2Int d)

deBruijn2Int :: DeBruijn -> Int
deBruijn2Int O = 0
deBruijn2Int (S n) = 1 + deBruijn2Int n

int2DeBruijn :: Int -> DeBruijn
int2DeBruijn 0 = O
int2DeBruijn n = S$int2DeBruijn (n-1)

-- natural size
naturalSizeSC :: SwissCheese -> Int
naturalSizeSC (IndexSC O) = 1
naturalSizeSC (IndexSC (S n)) = 1 + naturalSizeSC (IndexSC n)
naturalSizeSC (AbsSC t) = naturalSizeSC t + 1
naturalSizeSC (AppSC t1 t2) = naturalSizeSC t1 + naturalSizeSC t2 + 1
naturalSizeSC (Box _) = 0 

-- for generating affine and linear SwissCheese

-- Given a level and a SwissCheese, make all the SwissCheeses obtained by replacing
-- a □i by Index i and raising the level of the boxes
replaceBox :: Int -> SwissCheese -> [SwissCheese]
replaceBox i (c@(IndexSC _)) = []
replaceBox i (Box j) = if j == i then [IndexSC (int2DeBruijn i)] else []
replaceBox i (AbsSC t) = map AbsSC (replaceBox i t)
replaceBox i (AppSC t1 t2) =
  let lt1 = map (\t->AppSC t t2) (replaceBox i t1)
      lt2 = map (\t->AppSC t1 t) (replaceBox i t2)
  in lt1 ++ lt2

-- raise the boxes
raise :: SwissCheese -> SwissCheese
raise (Box j) = Box (j+1)
raise (c@(IndexSC _)) = c
raise (AbsSC c) = AbsSC (raise c)
raise (AppSC c1 c2) = AppSC (raise c1) (raise c2)

-- abstract a box at level i after replacing it and raising the other boxes
abstract :: Int -> SwissCheese -> [SwissCheese]
abstract i c = map (AbsSC . raise) (replaceBox i c)

-- App on a pair of SwissCheese
appSC :: [(SwissCheese,SwissCheese)] -> [SwissCheese]
appSC l = map (\(c1,c2) -> AppSC c1 c2) l

-- treelike memory containing SwissCheeses
data MemSC = MemSC [MemSC]
           | LoadSC [[SwissCheese]]

-- accessing a memory containing SwissCheese
accessSC :: MemSC -> Int -> [Int] -> [SwissCheese]
accessSC (LoadSC l) n []  = l !! n
accessSC (MemSC listM) n (k:m) = accessSC (listM !! k) n m

--- Local Variables:
--- mode: haskell
--- mode: haskell-indentation
--- End:
