minList::[Integer]->Integer
minList [] = 0
minList [x] = x
minList (x:xs) = min x (minList xs)

addAbs::[Integer]->Integer
addAbs [] = 0
addAbs (x:xs) = abs x + addAbs xs

existsOdd::[Integer]->Bool
existsOdd [] = False
existsOdd (x:xs) = odd x || existsOdd xs

findOdd::[Integer]-> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) = if odd x 
    then Just x
    else findOdd xs

removeEmpty::[String]->[String]
removeEmpty [] = []
removeEmpty (x:xs) = if null x
    then removeEmpty xs
    else x : removeEmpty xs

subtractEach::[(Integer, Integer)]->[Integer]
subtractEach [] = []
subtractEach ((a,b):xs) = (a - b) : subtractEach xs

makeGreeting::Maybe String->String
makeGreeting Nothing = "Hello!"
makeGreeting (Just x) = "Hello, " ++ x ++ "!"

catMaybes::[Maybe a]->[a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x:catMaybes xs

classify::[Either a b]->([a], [b])
classify [] = ([], [])
classify (Left a:xs) = 
    let (ls, rs) = classify xs
    in (a : ls, rs)
classify (Right b :xs) = 
    let (ls, rs) = classify xs
    in (ls, b:rs)

isPrefix::(Eq a) => [a]->[a]->Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) =
    if x == y
        then isPrefix xs ys
        else False
