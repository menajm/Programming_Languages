map::(a->b)->[c]->[b]
map f [] = []
map f (x:xs) = f x:map f xs


--Combinatory Logic

-- Precursor to lambda calculus
-- First turing complete programming language
-- Context free grammar
-- V is an infinite set of variables
    -- x, y, z, are elements of V

-- C is the set of combinadors
    -- = V | CC | I | K | S | .. | B | C |

-- Level of parse trees
    -- x, z, y, .. element of C
    -- xy 
--               / \
--    /\        x
--    xy          /  \ 
--                y  z

-- Every constant has one equation
    -- You cna only drop parantheses on the left side
-- (S(xk))((Iz)BI)
-- Turns into S(xk)(IZ (BI))

-- In general, we do not have the associativity rule
    -- X(YX) != (XY)Z

-- Also, we do not have communitivity 
    -- XY != YX

-- -------------Equations ----------
-- Refer to notes