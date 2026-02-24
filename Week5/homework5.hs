-- The type LTree has one leaf and one binary node constructor. The data values
--  occur in both.
data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving (Eq,Show)

-- The type MTree has one leaf, one unary node constructor, and one binary node
--  constructor. The data occurs in leaves and unary nodes only.
data MTree a = MLeaf a | UNode a (MTree a) | BNode (MTree a) (MTree a)
    deriving (Eq,Show)

-- ---Recursive Programming with Trees---

-- Question 1
getLLeaves::LTree a -> [a]
getLLeaves (LLeaf x) = [x]
getLLeaves (LNode _ l r)= getLLeaves l ++ getLLeaves r

getMLeaves::MTree a -> [a]
getMLeaves (MLeaf x) = [x]
getMLeaves (UNode _ u) = getMLeaves u
getMLeaves (BNode l r) = getMLeaves l ++ getMLeaves r

-- Question 2
maxLDepth::LTree a->Integer
maxLDepth (LLeaf _) = 0
maxLDepth (LNode _ l r) = 1 + max (maxLDepth l) (maxLDepth r)

maxMDepth::MTree a->Integer
maxMDepth (MLeaf _) = 0
maxMDepth (UNode _ u) = 1 + maxMDepth u
maxMDepth (BNode l r) = 1 + max (maxMDepth l) (maxMDepth r)

-- Question 3
maxLTree::LTree Integer->Integer
maxLTree (LLeaf x) = x
maxLTree (LNode x l r) = max x (max (maxLTree l) (maxLTree r))

maxMTree::MTree Integer->Integer
maxMTree (MLeaf x) = x
maxMTree (UNode x u) = max x (maxMTree u)
maxMTree (BNode l r) = max (maxMTree l) (maxMTree r)

-- Question 4
uncoveredLeafL::Integer->LTree Integer->Bool
uncoveredLeafL n tree = checkIfUncovered False tree
    where
        checkIfUncovered haveSeenN (LLeaf x)
            |x == n && not haveSeenN = True
            |otherwise  = False

        checkIfUncovered haveSeenN (LNode x l r) =
            let newHaveSeenN = haveSeenN || (x == n)
            in checkIfUncovered newHaveSeenN l || checkIfUncovered newHaveSeenN r

uncoveredLeafM::Integer->MTree Integer->Bool
uncoveredLeafM n tree = checkIfUncovered False tree
    where
        checkIfUncovered haveSeen (MLeaf x)
            |x == n && not haveSeen = True
            |otherwise  = False
        
        checkIfUncovered haveSeen (UNode x t) =
            let newHaveSeen = haveSeen || (x==n)
            in checkIfUncovered newHaveSeen t
        
        checkIfUncovered haveSeen (BNode l r) =
            checkIfUncovered haveSeen l || checkIfUncovered haveSeen r

-- Question 5
mapLTree::(a->b)->LTree a->LTree b
mapLTree f (LLeaf x) = LLeaf(f x) 
mapLTree f (LNode x l r) =
    LNode (f x) (mapLTree f l) (mapLTree f r) 

mapMTree::(a->b)->MTree a->MTree b
mapMTree f (MLeaf x) = MLeaf (f x)
mapMTree f (UNode x u) = UNode (f x) (mapMTree f u)
mapMTree f (BNode l r) = BNode (mapMTree f l) (mapMTree f r)

-- Question 6
applyLfun::LTree Integer->LTree Integer
applyLfun = mapLTree (\x -> 2^(x^2) - x)

applyMfun::MTree Integer->MTree Integer
applyMfun = mapMTree (\x -> 2^(x^2) - x)

-- Question 7

-- Helper function
orMaybes :: Maybe a-> Maybe a-> Maybe a
orMaybes Nothing y = y
orMaybes x _ = x

findLTree::(a->Bool)->LTree a->Maybe a
findLTree p (LLeaf x) = if p x then Just x else Nothing
findLTree p (LNode x l r)
    | p x    = Just x
    | otherwise = orMaybes (findLTree p l) (findLTree p r)


findMTree::(a->Bool)->MTree a->Maybe a
findMTree p (MLeaf x) = if p x then Just x else Nothing
findMTree p (UNode x u)
    |p x = Just x
    | otherwise = findMTree p u
findMTree p (BNode l r) =
    orMaybes (findMTree p l) (findMTree p r)

-- Question 8
findLpali::LTree String->Maybe String
findLpali = findLTree (\x -> x == reverse x)

findMpali::MTree String->Maybe String
findMpali = findMTree (\x-> x == reverse x)

-- Question 9

foldLTree :: (a-> b-> b-> b)-> (a-> b)-> LTree a-> b
foldLTree n l (LLeaf x) = l x
foldLTree n l (LNode x t1 t2) = n x (foldLTree n l t1) (foldLTree n l t2)

foldMTree :: (b-> b-> b)-> (a-> b-> b)-> (a-> b)-> MTree a-> b
foldMTree n u l (MLeaf x) = l x
foldMTree n u l (UNode x t) = u x (foldMTree n u l t)
foldMTree n u l (BNode t1 t2) = n (foldMTree n u l t1) (foldMTree n u l t2)


getLLeaves'::LTree a->[a]
getLLeaves' = foldLTree (\_ l r -> l ++ r) (\x->[x])

getMLeaves'::MTree a->[a]
getMLeaves' = foldMTree (++) (\_ subtree -> subtree) (\x -> [x])

-- Question 10
uncoveredLeafL'::Integer->LTree Integer->Bool
uncoveredLeafL' n tree = (foldLTree nodeCase leafCase tree) False
    where
        leafCase x haveSeen =
            x == n && not haveSeen
        nodeCase x l r haveSeen =
            let newSeen = haveSeen || (x == n)
            in l newSeen || r newSeen

uncoveredLeafM'::Integer->MTree Integer->Bool
uncoveredLeafM' n tree = (foldMTree bCase uCase leafCase tree) False
    where 
        leafCase x haveSeen =
            x == n && not haveSeen
        uCase x subtree haveSeen =
            let newSeen = haveSeen || (x == n)
            in subtree newSeen
        bCase l r haveSeen =
            l haveSeen || r haveSeen


-- ---Sample Output ---
exLTree :: LTree Integer
exLTree = LNode 5 (LLeaf 4)
    (LNode 3 (LNode 2 (LLeaf 5) (LLeaf 1))
    (LLeaf 7))
exMTree :: MTree Integer
exMTree = BNode (UNode 5 (BNode (MLeaf 1) (MLeaf 10)))
    (BNode (UNode 3 (MLeaf 3)) (UNode 4 (MLeaf 3)))