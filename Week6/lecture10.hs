-- DEFINITIONS
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show
data TTree a = TLeaf a |UNode a (TTree a)
    | TNode (TTree a) (TTree a) (TTree a)
    deriving Show
data FTree a = FNode a [FTree a]
    deriving Show



-- ------------------- EXAMPLES -------------------
listifyTree::Tree a->[a]
listifyTree Leaf = []
listifyTree (Node x t1 t2) = x : listifyTree t1 ++ listify t2

listifyTTree::TTree a->[a]
listifyTTree (TLeaf x) = [a]
listifyTTree (UNode x t) = x : listifyTTree t
listifyTTree (TNode t1 t2 t3) = listifyTTree t1 ++ listifyTTree t2 ++ listifyTTree t3

listifyFTree::FTree a->[a]
listifyFTree (FNode x ts) = x : foldr (++) [] (map listifyFTree ts)



mapTree::(a->b)->Tree a->Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

mapTTree::(a->b)->TTree a->TTree b
mapTTree f (TLeaf x) = TLeaf (f x)
mapTTree f (UNode x t) = UNode (f x) (mapTTree f t)
mapTTree f (TNode t1 t2 t3) = TNode (mapTTree f t1) (mapTTree f t2) (mapTTree f t3)

mapFTree::(a->b)->FTree a->FTree b
-- Find a way to make a ftree of type a (ts) to type b
mapFTree f (FNode x ts) = FNode (f x) (map (mapFTree f) ts)



-- --------------- FOLDS AND TREES ---------------
foldTree:: (b)->(a->b->b->b)->Tree a-> b
foldTree bc rf Leaf = bc
foldTree bc rf (Node x t1 t2) = rf x (foldTree bc rf t1) (foldTree bc rf t2)

foldTTree::(a->b)->(a->b->b)->(b->b->b->b)->TTree a-> b
foldTTree bc un tn (TLeaf x) = bc x
foldTTree bc un tn (UNode x t) = un x (foldTTree bc un tn t)
foldTTree bc un tn (TNode t1 t2 t3) = tn y1 y2 y3
    where [y1, y2, y3] = map (foldTTree bc un tn) [t1, t2, t3]



-- --------------- EXAMPLE STRUCTURE ---------------
exTree::Tree Integer
exTree=Node 3 (Node 1 Leaf Leaf)
                (Node 5(Node 4 Leaf Leaf)(Node 6 Leaf Leaf))

exTTree::Tree Integer
exTTree=UNode 5 (TNode (TLeaf (-1))
                        (UNode 6 (TLeaf 2))
                (TNode (Tleaf 10) (TLeaf 11) (TLeaf 12)))

exFTree::FTree Integer
exFTree = FNode 3[FNode 1 [],
            FNode 7[FNode 0 [],
                FNode 6 [FNode 1 [],
                        FNode 2 []],
                FNode (-1) [],
                FNode 0 [FNode 7 []] ] ]