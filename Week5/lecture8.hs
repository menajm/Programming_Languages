-- Actually lecture 9
data List a = Nil | Cons a (List a)
    deriving Show
-- [1, 2, 3] = 1:2:3:[]
--  []::[a]
--      (:)->a->[a]->[a]

-- List a ::=[]|a:List a

ex1::List Integer   -- [1, 2, 3]
ex1 = Cons 1 (Cons 2(Cons 3 Nil))
append::List a->List a->List a
append Nil ys = ys
append (Cons x xs)ys = Cons x (append xs ys)

-- Trees
data Tree a = Leaf | Node a (Tree a)(Tree a)
    deriving Show

ex2::Tree Integer
ex2 = Node 5 (Node 3 (Node 6 Leaf Leaf)
                        (Node 7 Leaf Leaf)) 
            (Node (-1) Leaf Leaf)
-- 5 is at the root and it has two subtrees
-- We include the empty part of the "bottom" with just Leaf

addTree::Tree Integer->Integer
-- Refer to the data Tree part when using the function v
addTree Leaf = 0
addTree (Node x t1 t2) = x + addTree t1 + addTree t2   -- You have a left and a right subtree so there is t1 and t2

-- Check for an even element in the tree
checkEven::Tree Integer->Bool
checkEven Leaf = False
checkEven (Node x t1 t2) = if even x then True else (checkEven t1) || (checkEven t2)
    -- Or you can use even x || checkEven t1 || checkEven t2


checkTree::(a->Bool)->Tree a->Bool
checkTree p Leaf = False
checkTree p (Node x t1 t2) = p x || checkTree p t1 || checkTree p t2

findTree::(a->Bool)->Tree a->Maybe a
findTree p Leaf = Nothing
findTree p (Node x t1 t2) = if p x then Just x
    else case findTree p t1 of
        Nothing-> findTree p t2
        Just y -> Just y


-- Using higher order functions

mapTree::(a->b)->Tree a->Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1)(mapTree f t2)
-- You can map length, reverse, etc.