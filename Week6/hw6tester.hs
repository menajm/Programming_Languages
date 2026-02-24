subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (flip elem ys) xs

eq :: (Eq a) => [a] -> [a] -> Bool
eq xs ys = subset xs ys && subset ys xs

listelem :: (Eq a) => [a] -> [[a]] -> Bool
listelem xs [] = False
listelem xs (y:ys) = xs `eq` y || listelem xs ys

listsubset :: (Eq a) => [[a]] -> [[a]] -> Bool
listsubset [] ys = True
listsubset (x:xs) ys = listelem x ys && listsubset xs ys

listeq :: (Eq a) => [[a]] -> [[a]] -> Bool
listeq l1 l2 = listsubset l1 l2 && listsubset l2 l1

tests =
  [ ((mapAppend show [1,23,456]) == "123456")
  , ((mapAppend (\c -> c : " ") "hello") == "h e l l o ")
  , ((mapAppend (\s -> [take i s | i <- [1..3]]) ["hello","there","world"]) == ["h","he","hel","t","th","the","w","wo","wor"])
  , ((mapAppend (\s -> [elem c "aeiou" | c <- s]) ["hello","there","world"]) == [False,True,False,False,True,False,False,True,False,True,False,True,False,False,False])
  , ((mapAppend (\s -> [(c,i) | (c, i) <- zip s [1..]]) ["hello","again","world"]) == [('h',1),('e',2),('l',3),('l',4),('o',5),('a',1),('g',2),('a',3),('i',4),('n',5),('w',1),('o',2),('r',3),('l',4),('d',5)])
  , ((mapAppend (\x -> [1..x]) [1..5]) == [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5])
  , ((mapAppend (\x -> [x | y <- [1..x]]) [1..5]) == [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5])
  , ((mapAppend (\(x,y) -> [z | z <- [x..y]]) [(1,2),(4,5),(9,0),(8,2),(11,15)]) == [1,2,4,5,11,12,13,14,15])
  , ((mapAppend (\x -> [x - y | y <- [1..x]]) [1..3]) == [0,1,0,2,1,0])
  , ((mapAppend (\x -> [(x,y) | y <- (reverse ['A'..'C'])]) ['a'..'c']) == [('a','C'),('a','B'),('a','A'),('b','C'),('b','B'),('b','A'),('c','C'),('c','B'),('c','A')])
  , ((addLetter 'x' ["hey","again"]) == ["xhey","xagain"])
  , ((addLetter 'h' ["it","ill"]) == ["hit","hill"])
  , ((addLetter 't' ["ower","all","ale"]) == ["tower","tall","tale"])
  , ((addLetter 'b' ["ear","ent","old"]) == ["bear","bent","bold"])
  , ((addLetter 'l' ["air","earn","aw"]) == ["lair","learn","law"])
  , ((addLetter 'x' [""]) == ["x"])
  , ((addLetter 'x' []) == [])
  , ((addLetter 'x' ["hack"]) == ["xhack"])
  , ((addLetter 'x' ["a","b","c","d","e"]) == ["xa","xb","xc","xd","xe"])
  , ((addLetter 'x' ["avier","-ray","ylophone"]) == ["xavier","x-ray","xylophone"])
  , ((addLetters "abc" ["hey","again"]) == ["ahey","aagain","bhey","bagain","chey","cagain"])
  , ((addLetters "xyz" [""]) == ["x","y","z"])
  , ((addLetters "abc" ["1","2","3"]) == ["a1","a2","a3","b1","b2","b3","c1","c2","c3"])
  , ((addLetters "rt" ["est","on","ower"]) == ["rest","ron","rower","test","ton","tower"])
  , ((addLetters "bp" ["art","ear","ow","un"]) == ["bart","bear","bow","bun","part","pear","pow","pun"])
  , ((addLetters "ab" ["hello"]) == ["ahello","bhello"])
  , ((addLetters "hi" ["happy","hacking","!"]) == ["hhappy","hhacking","h!","ihappy","ihacking","i!"])
  , ((addLetters "xyzw" ["a","b"]) == ["xa","xb","ya","yb","za","zb","wa","wb"])
  , ((addLetters "world" ["hello"]) == ["whello","ohello","rhello","lhello","dhello"])
  , ((addLetters "big" ["part","one"]) == ["bpart","bone","ipart","ione","gpart","gone"])
  , ((makeWords "abc" 2) == ["aa","ab","ac","ba","bb","bc","ca","cb","cc"])
  , ((makeWords "ab" 4) == ["aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb","baaa","baab","baba","babb","bbaa","bbab","bbba","bbbb"])
  , ((makeWords "xyz" 3) == ["xxx","xxy","xxz","xyx","xyy","xyz","xzx","xzy","xzz","yxx","yxy","yxz","yyx","yyy","yyz","yzx","yzy","yzz","zxx","zxy","zxz","zyx","zyy","zyz","zzx","zzy","zzz"])
  , ((makeWords "z" 0) == [""])
  , ((makeWords "char" 1) == ["c","h","a","r"])
  , ((makeWords "bravo" 2) == ["bb","br","ba","bv","bo","rb","rr","ra","rv","ro","ab","ar","aa","av","ao","vb","vr","va","vv","vo","ob","or","oa","ov","oo"])
  , ((makeWords "a" 3) == ["aaa"])
  , ((makeWords "123" 3) == ["111","112","113","121","122","123","131","132","133","211","212","213","221","222","223","231","232","233","311","312","313","321","322","323","331","332","333"])
  , ((makeWords "ijk" 4) == ["iiii","iiij","iiik","iiji","iijj","iijk","iiki","iikj","iikk","ijii","ijij","ijik","ijji","ijjj","ijjk","ijki","ijkj","ijkk","ikii","ikij","ikik","ikji","ikjj","ikjk","ikki","ikkj","ikkk","jiii","jiij","jiik","jiji","jijj","jijk","jiki","jikj","jikk","jjii","jjij","jjik","jjji","jjjj","jjjk","jjki","jjkj","jjkk","jkii","jkij","jkik","jkji","jkjj","jkjk","jkki","jkkj","jkkk","kiii","kiij","kiik","kiji","kijj","kijk","kiki","kikj","kikk","kjii","kjij","kjik","kjji","kjjj","kjjk","kjki","kjkj","kjkk","kkii","kkij","kkik","kkji","kkjj","kkjk","kkki","kkkj","kkkk"])
  , ((makeWords "three" 3) == ["ttt","tth","ttr","tte","tte","tht","thh","thr","the","the","trt","trh","trr","tre","tre","tet","teh","ter","tee","tee","tet","teh","ter","tee","tee","htt","hth","htr","hte","hte","hht","hhh","hhr","hhe","hhe","hrt","hrh","hrr","hre","hre","het","heh","her","hee","hee","het","heh","her","hee","hee","rtt","rth","rtr","rte","rte","rht","rhh","rhr","rhe","rhe","rrt","rrh","rrr","rre","rre","ret","reh","rer","ree","ree","ret","reh","rer","ree","ree","ett","eth","etr","ete","ete","eht","ehh","ehr","ehe","ehe","ert","erh","err","ere","ere","eet","eeh","eer","eee","eee","eet","eeh","eer","eee","eee","ett","eth","etr","ete","ete","eht","ehh","ehr","ehe","ehe","ert","erh","err","ere","ere","eet","eeh","eer","eee","eee","eet","eeh","eer","eee","eee"])
  , ((update (3, "food") []) == [(3,"food")])
  , ((update (2, "new") [(2, "old")]) == [(2,"new")])
  , ((update (3, "food") [(2, "old")]) == [(2,"old"),(3,"food")])
  , ((update (3, "food") [(2, "old"), (3, "also_old")]) == [(2,"old"),(3,"food")])
  , ((update (3, "food") [(2, "food"), (3, "also_old")]) == [(2,"food"),(3,"food")])
  , ((update (1, "glue") [(3, "food"), (1, "blue"), (2, "also_old")]) == [(3,"food"),(1,"glue"),(2,"also_old")])
  , ((update (2, "glue") [(3, "food"), (1, "blue"), (2, "also_old")]) == [(3,"food"),(1,"blue"),(2,"glue")])
  , ((update ("key", "value") [("key", "bad_value"), ("key2", "")]) == [("key","value"),("key2","")])
  , ((update (3, 4) [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7)]) == [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7)])
  , ((update (3, 5) [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7)]) == [(1,2),(2,3),(3,5),(4,5),(5,6),(6,7)])
  , ((fv (Var "X" `And` Var "Y")) `eq` ["X","Y"])
  , ((fv (Var "X" `And` Const True `Or` Var "Y")) `eq` ["X","Y"])
  , ((fv ((Var "X" `And` Var "Y") `And` (Var "Z"))) `eq` ["X","Y","Z"])
  , ((fv ((Not (Var "X") `Or` Not (Var "Z")) `And` (Not (Var "Y") `Or` Var "Z"))) `eq` ["X","Z","Y"])
  , ((fv (Not (Var "X") `Or` (Var "X") `And` Var "Y")) `eq` ["X","Y"])
  , ((fv ((Const True `And` Const False) `Or` Var "X")) `eq` ["X"])
  , ((fv ((Const True `And` Const False) `Or` Not (Var "X"))) `eq` ["X"])
  , ((fv (Var "Z" `Or` Var "X" `And` (Not (Var "W" `Or` Var "Y")))) `eq` ["Z","X","W","Y"])
  , ((fv (Var "Z" `Or` Const True `And` (Not (Var "W" `Or` Var "Y")))) `eq` ["Z","W","Y"])
  , ((fv ((Const True `And` Const False) `Or` Const True)) `eq` [])
  , ((countOccurs "X" (Var "Y")) == 0)
  , ((countOccurs "X" (Var "X")) == 1)
  , ((countOccurs "X" (And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))) == 2)
  , ((countOccurs "Z" (And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))) == 1)
  , ((countOccurs "A" (And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))) == 0)
  , ((countOccurs "X" (Not (Not (Not (Var "X"))))) == 1)
  , ((countOccurs "Z" (Not (Not (Not (Var "X"))))) == 0)
  , ((countOccurs "X" (Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))) == 2)
  , ((countOccurs "Y" (Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))) == 2)
  , ((countOccurs "Z" (Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))) == 2)
  , ((setTrue "X" (Var "Y")) == Var "Y")
  , ((setTrue "X" (Var "X")) == Const True)
  , ((setTrue "X" (And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))) == And (And (Var "Y") (Var "Z")) (Or (Const True) (Const True)))
  , ((setTrue "Z" (And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))) == And (And (Var "Y") (Const True)) (Or (Var "X") (Var "X")))
  , ((setTrue "A" (And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))) == And (And (Var "Y") (Var "Z")) (Or (Var "X") (Var "X")))
  , ((setTrue "X" (Not (Not (Not (Var "X"))))) == Not (Not (Not (Const True))))
  , ((setTrue "Z" (Not (Not (Not (Var "X"))))) == Not (Not (Not (Var "X"))))
  , ((setTrue "X" (Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))) == Or (And (Const True) (Or (And (Var "Z") (Var "Z")) (Not (And (Const True) (Var "Y"))))) (Not (Var "Y")))
  , ((setTrue "Y" (Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))) == Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Const True))))) (Not (Const True)))
  , ((setTrue "Z" (Or (And (Var "X") (Or (And (Var "Z") (Var "Z")) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))) == Or (And (Var "X") (Or (And (Const True) (Const True)) (Not (And (Var "X") (Var "Y"))))) (Not (Var "Y")))
  , ((lookUp "X" [("X",True),("Y",False)]) == True)
  , ((lookUp "X" [("W",False),("X",True),("Y",False)]) == True)
  , ((lookUp "Y" [("W",False),("X",True),("Y",False)]) == False)
  , ((lookUp "Z" [("W",False),("X",True),("Y",False),("Z",False)]) == False)
  , ((lookUp "Z" [("W",False),("X",True),("Y",False),("Z",True)]) == True)
  , ((lookUp "Y" [("X",True),("Y",False)]) == False)
  , ((lookUp "W" [("W",False),("X",True),("Y",False)]) == False)
  , ((lookUp "W" [("W",True),("X",True),("Y",False)]) == True)
  , ((lookUp "Z" [("W",False),("Z",False),("X",True),("Y",False)]) == False)
  , ((lookUp "Z" [("W",False),("X",True),("Z",True),("Y",False)]) == True)
  , ((eval [("X",False),("Y",True) ] (Var "X" `And` Var "Y")) == False)
  , ((eval [("X",False),("Y",True) ] (Var "X" `Or` Var "Y")) == True)
  , ((eval [("X",False),("Y",True) ] (Not (Var "X") `Or` (Var "Y"))) == True)
  , ((eval [("X",True) ,("Y",False)] (Not (Var "X") `Or` (Var "Y"))) == False)
  , ((eval [("X",True) ,("Y",True) ] (Not (Var "X") `Or` Not (Var "Y"))) == False)
  , ((eval [("X",False),("Y",True) ,("Z",False)] (Var "X" `And` Var "Y")) == False)
  , ((eval [("X",False),("Y",True) ,("Z",False)] (Var "Z" `Or` Var "Y")) == True)
  , ((eval [("X",False),("Y",True) ,("Z",True)] (Var "Z")) == True)
  , ((eval [("X",True) ,("Y",False),("W",False)] (Not (Var "X") `Or` (Var "Y" `And` Var "W"))) == False)
  , ((eval [("X",True) ,("Y",True),("W",False)] (Not (Var "X") `Or` Not (Var "Y" `And` Var "W"))) == True)
  , ((evalList (Var "X" `And` Var "Y") [[("X",False),("Y",True)]]) == False)
  , ((evalList (Var "X" `And` Var "Y") [[("X",False),("Y",True)],[("X",True),("Y",True)]]) == True)
  , ((evalList (Var "X" `Or` Var "Y") [[("X",False),("Y",True)]]) == True)
  , ((evalList (Var "X" `Or` Var "Y") []) == False)
  , ((evalList (Not (Var "X") `Or` Not (Var "Y")) [[("X",False),("Y",True)],[("X",True),("Y",True)]]) == True)
  , ((evalList (Not (Var "X") `Or` Not (Var "Y")) [[("X",True) ,("Y",True)]]) == False)
  , ((evalList (Const True) [[("X",False),("Y",False)],[("X",True),("Y",True)]]) == True)
  , ((evalList (Const False) [[("X",False),("Y",False)],[("X",True),("Y",True)]]) == False)
  , ((evalList (Var "X" `And` Var "Y" `And` Var "Z") [[("X",False),("Y",True),("Z",False)],[("X",True),("Y",True),("Z",False)]]) == False)
  , ((evalList (Var "X" `And` Var "Y" `And` Var "Z") [[("X",False),("Y",True),("Z",False)],[("X",True),("Y",True),("Z",True)],[("X",True),("Y",True),("Z",False)]]) == True)
  , ((extendEnv [] "X") `listeq` [])
  , ((extendEnv [[("X", True)]] "") `listeq` [[("",False),("X",True)],[("",True),("X",True)]])
  , ((extendEnv [] "") `listeq` [])
  , ((extendEnv [[("X", False), ("Y", True)], []] "Z") `listeq` [[("Z",False),("X",False),("Y",True)],[("Z",False)],[("Z",True),("X",False),("Y",True)],[("Z",True)]])
  , ((extendEnv [[("X", False), ("Y", True)], [("A", False)]] "Z") `listeq` [[("Z",False),("X",False),("Y",True)],[("Z",False),("A",False)],[("Z",True),("X",False),("Y",True)],[("Z",True),("A",False)]])
  , ((extendEnv [[("D", False)], [("B", False), ("T", True)]] "A") `listeq` [[("A",False),("D",False)],[("A",False),("B",False),("T",True)],[("A",True),("D",False)],[("A",True),("B",False),("T",True)]])
  , ((extendEnv [[], [("X", False), ("C", True), ("A", False)]] "D") `listeq` [[("D",False)],[("D",False),("X",False),("C",True),("A",False)],[("D",True)],[("D",True),("X",False),("C",True),("A",False)]])
  , ((extendEnv [[("X", False), ("C", True), ("A", False)], []] "D") `listeq` [[("D",False),("X",False),("C",True),("A",False)],[("D",False)],[("D",True),("X",False),("C",True),("A",False)],[("D",True)]])
  , ((extendEnv [[("C", False), ("D", True), ("E", False), ("F", False), ("G", True)], [("A", False)]] "B") `listeq` [[("B",False),("C",False),("D",True),("E",False),("F",False),("G",True)],[("B",False),("A",False)],[("B",True),("C",False),("D",True),("E",False),("F",False),("G",True)],[("B",True),("A",False)]])
  , ((extendEnv [[("A", False)], [("C", True), ("D", False), ("E", True), ("F", False), ("G", False)]] "B") `listeq` [[("B",False),("A",False)],[("B",False),("C",True),("D",False),("E",True),("F",False),("G",False)],[("B",True),("A",False)],[("B",True),("C",True),("D",False),("E",True),("F",False),("G",False)]])
  , ((genEnvs ["X"]) `listeq` [[("X",False)],[("X",True)]])
  , ((genEnvs ["X","Y"]) `listeq` [[("X",False),("Y",False)],[("X",False),("Y",True)],[("X",True),("Y",False)],[("X",True),("Y",True)]])
  , ((genEnvs []) `listeq` [[]])
  , ((genEnvs ["X","Y","Z"]) `listeq` [[("X",False),("Y",False),("Z",False)],[("X",False),("Y",False),("Z",True)],[("X",False),("Y",True),("Z",False)],[("X",False),("Y",True),("Z",True)],[("X",True),("Y",False),("Z",False)],[("X",True),("Y",False),("Z",True)],[("X",True),("Y",True),("Z",False)],[("X",True),("Y",True),("Z",True)]])
  , ((genEnvs ["X","Y","Z","W"]) `listeq` [[("X",False),("Y",False),("Z",False),("W",False)],[("X",False),("Y",False),("Z",False),("W",True)],[("X",False),("Y",False),("Z",True),("W",False)],[("X",False),("Y",False),("Z",True),("W",True)],[("X",False),("Y",True),("Z",False),("W",False)],[("X",False),("Y",True),("Z",False),("W",True)],[("X",False),("Y",True),("Z",True),("W",False)],[("X",False),("Y",True),("Z",True),("W",True)],[("X",True),("Y",False),("Z",False),("W",False)],[("X",True),("Y",False),("Z",False),("W",True)],[("X",True),("Y",False),("Z",True),("W",False)],[("X",True),("Y",False),("Z",True),("W",True)],[("X",True),("Y",True),("Z",False),("W",False)],[("X",True),("Y",True),("Z",False),("W",True)],[("X",True),("Y",True),("Z",True),("W",False)],[("X",True),("Y",True),("Z",True),("W",True)]])
  , ((genEnvs (map (\c -> c : "") ['a'..'e'])) `listeq` [[("a",False),("b",False),("c",False),("d",False),("e",False)],[("a",False),("b",False),("c",False),("d",False),("e",True)],[("a",False),("b",False),("c",False),("d",True),("e",False)],[("a",False),("b",False),("c",False),("d",True),("e",True)],[("a",False),("b",False),("c",True),("d",False),("e",False)],[("a",False),("b",False),("c",True),("d",False),("e",True)],[("a",False),("b",False),("c",True),("d",True),("e",False)],[("a",False),("b",False),("c",True),("d",True),("e",True)],[("a",False),("b",True),("c",False),("d",False),("e",False)],[("a",False),("b",True),("c",False),("d",False),("e",True)],[("a",False),("b",True),("c",False),("d",True),("e",False)],[("a",False),("b",True),("c",False),("d",True),("e",True)],[("a",False),("b",True),("c",True),("d",False),("e",False)],[("a",False),("b",True),("c",True),("d",False),("e",True)],[("a",False),("b",True),("c",True),("d",True),("e",False)],[("a",False),("b",True),("c",True),("d",True),("e",True)],[("a",True),("b",False),("c",False),("d",False),("e",False)],[("a",True),("b",False),("c",False),("d",False),("e",True)],[("a",True),("b",False),("c",False),("d",True),("e",False)],[("a",True),("b",False),("c",False),("d",True),("e",True)],[("a",True),("b",False),("c",True),("d",False),("e",False)],[("a",True),("b",False),("c",True),("d",False),("e",True)],[("a",True),("b",False),("c",True),("d",True),("e",False)],[("a",True),("b",False),("c",True),("d",True),("e",True)],[("a",True),("b",True),("c",False),("d",False),("e",False)],[("a",True),("b",True),("c",False),("d",False),("e",True)],[("a",True),("b",True),("c",False),("d",True),("e",False)],[("a",True),("b",True),("c",False),("d",True),("e",True)],[("a",True),("b",True),("c",True),("d",False),("e",False)],[("a",True),("b",True),("c",True),("d",False),("e",True)],[("a",True),("b",True),("c",True),("d",True),("e",False)],[("a",True),("b",True),("c",True),("d",True),("e",True)]])
  , ((genEnvs ["a","b"]) `listeq` [[("a",False),("b",False)],[("a",False),("b",True)],[("a",True),("b",False)],[("a",True),("b",True)]])
  , ((genEnvs ["a","x","b","y"]) `listeq` [[("a",False),("x",False),("b",False),("y",False)],[("a",False),("x",False),("b",False),("y",True)],[("a",False),("x",False),("b",True),("y",False)],[("a",False),("x",False),("b",True),("y",True)],[("a",False),("x",True),("b",False),("y",False)],[("a",False),("x",True),("b",False),("y",True)],[("a",False),("x",True),("b",True),("y",False)],[("a",False),("x",True),("b",True),("y",True)],[("a",True),("x",False),("b",False),("y",False)],[("a",True),("x",False),("b",False),("y",True)],[("a",True),("x",False),("b",True),("y",False)],[("a",True),("x",False),("b",True),("y",True)],[("a",True),("x",True),("b",False),("y",False)],[("a",True),("x",True),("b",False),("y",True)],[("a",True),("x",True),("b",True),("y",False)],[("a",True),("x",True),("b",True),("y",True)]])
  , ((genEnvs ["alpha","gamma"]) `listeq` [[("alpha",False),("gamma",False)],[("alpha",False),("gamma",True)],[("alpha",True),("gamma",False)],[("alpha",True),("gamma",True)]])
  , ((genEnvs ["v1","v2","v3"]) `listeq` [[("v1",False),("v2",False),("v3",False)],[("v1",False),("v2",False),("v3",True)],[("v1",False),("v2",True),("v3",False)],[("v1",False),("v2",True),("v3",True)],[("v1",True),("v2",False),("v3",False)],[("v1",True),("v2",False),("v3",True)],[("v1",True),("v2",True),("v3",False)],[("v1",True),("v2",True),("v3",True)]])
  , ((sat (Const True)) == True)
  , ((sat (Var "X")) == True)
  , ((sat (Var "X" `And` Var "Y")) == True)
  , ((sat (Var "X" `And` (Not $ Var "X"))) == False)
  , ((sat (Var "X" `Or` (Not $ Var "X"))) == True)
  , ((sat (Var "X" `Or` (Var "Y" `And` Not (Var "X")))) == True)
  , ((sat (Var "Y" `And` (Not (Var "Y")) `Or` Var "X")) == True)
  , ((sat ((Not $ Var "X") `And` (Not $ Var "Y") `And` (Not $ Var "Z"))) == True)
  , ((sat (Not (Var "Y" `And` Not (Var "Y")))) == True)
  , ((sat (Var "X" `And` Const False `Or` (Not (Var "X") `Or` Var "Z"))) == True)
  , ((checkEq (Or (Var "P") (Var "P")) (Var "P")) == True)
  , ((checkEq (And (Var "P") (Var "P")) (Var "P")) == True)
  , ((checkEq (Or (Var "P") (Const True)) (Const True)) == True)
  , ((checkEq (And (Var "P") (Const False)) (Const False)) == True)
  , ((checkEq (Or (Var "P") (Not (Var "P"))) (Const False)) == False)
  , ((checkEq (Not (And (Var "P") (Not (Var "P")))) (Const True)) == True)
  , ((checkEq (Or (Var "P") (And (Var "P") (Var "Q"))) (Var "P")) == True)
  , ((checkEq (And (Var "P") (Or (Var "P") (Var "Q"))) (Var "Q")) == False)
  , ((checkEq (And (Var "P") (Or (Var "Q") (Var "R"))) (Or (And (Var "P") (Var "Q")) (And (Var "P") (Var "R")))) == True)
  , ((checkEq (Or (Var "P") (And (Var "Q") (Var "R"))) (And (Or (Var "P") (Var "Q")) (Or (Var "P") (Var "R")))) == True)
  ]
testLinesString =
  [ "(mapAppend show [1,23,456])"
  , "(mapAppend (\\c -> c : \" \") \"hello\")"
  , "(mapAppend (\\s -> [take i s | i <- [1..3]]) [\"hello\",\"there\",\"world\"])"
  , "(mapAppend (\\s -> [elem c \"aeiou\" | c <- s]) [\"hello\",\"there\",\"world\"])"
  , "(mapAppend (\\s -> [(c,i) | (c, i) <- zip s [1..]]) [\"hello\",\"again\",\"world\"])"
  , "(mapAppend (\\x -> [1..x]) [1..5])"
  , "(mapAppend (\\x -> [x | y <- [1..x]]) [1..5])"
  , "(mapAppend (\\(x,y) -> [z | z <- [x..y]]) [(1,2),(4,5),(9,0),(8,2),(11,15)])"
  , "(mapAppend (\\x -> [x - y | y <- [1..x]]) [1..3])"
  , "(mapAppend (\\x -> [(x,y) | y <- (reverse ['A'..'C'])]) ['a'..'c'])"
  , "(addLetter 'x' [\"hey\",\"again\"])"
  , "(addLetter 'h' [\"it\",\"ill\"])"
  , "(addLetter 't' [\"ower\",\"all\",\"ale\"])"
  , "(addLetter 'b' [\"ear\",\"ent\",\"old\"])"
  , "(addLetter 'l' [\"air\",\"earn\",\"aw\"])"
  , "(addLetter 'x' [\"\"])"
  , "(addLetter 'x' [])"
  , "(addLetter 'x' [\"hack\"])"
  , "(addLetter 'x' [\"a\",\"b\",\"c\",\"d\",\"e\"])"
  , "(addLetter 'x' [\"avier\",\"-ray\",\"ylophone\"])"
  , "(addLetters \"abc\" [\"hey\",\"again\"])"
  , "(addLetters \"xyz\" [\"\"])"
  , "(addLetters \"abc\" [\"1\",\"2\",\"3\"])"
  , "(addLetters \"rt\" [\"est\",\"on\",\"ower\"])"
  , "(addLetters \"bp\" [\"art\",\"ear\",\"ow\",\"un\"])"
  , "(addLetters \"ab\" [\"hello\"])"
  , "(addLetters \"hi\" [\"happy\",\"hacking\",\"!\"])"
  , "(addLetters \"xyzw\" [\"a\",\"b\"])"
  , "(addLetters \"world\" [\"hello\"])"
  , "(addLetters \"big\" [\"part\",\"one\"])"
  , "(makeWords \"abc\" 2)"
  , "(makeWords \"ab\" 4)"
  , "(makeWords \"xyz\" 3)"
  , "(makeWords \"z\" 0)"
  , "(makeWords \"char\" 1)"
  , "(makeWords \"bravo\" 2)"
  , "(makeWords \"a\" 3)"
  , "(makeWords \"123\" 3)"
  , "(makeWords \"ijk\" 4)"
  , "(makeWords \"three\" 3)"
  , "(update (3, \"food\") [])"
  , "(update (2, \"new\") [(2, \"old\")])"
  , "(update (3, \"food\") [(2, \"old\")])"
  , "(update (3, \"food\") [(2, \"old\"), (3, \"also_old\")])"
  , "(update (3, \"food\") [(2, \"food\"), (3, \"also_old\")])"
  , "(update (1, \"glue\") [(3, \"food\"), (1, \"blue\"), (2, \"also_old\")])"
  , "(update (2, \"glue\") [(3, \"food\"), (1, \"blue\"), (2, \"also_old\")])"
  , "(update (\"key\", \"value\") [(\"key\", \"bad_value\"), (\"key2\", \"\")])"
  , "(update (3, 4) [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7)])"
  , "(update (3, 5) [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7)])"
  , "(fv (Var \"X\" `And` Var \"Y\"))"
  , "(fv (Var \"X\" `And` Const True `Or` Var \"Y\"))"
  , "(fv ((Var \"X\" `And` Var \"Y\") `And` (Var \"Z\")))"
  , "(fv ((Not (Var \"X\") `Or` Not (Var \"Z\")) `And` (Not (Var \"Y\") `Or` Var \"Z\")))"
  , "(fv (Not (Var \"X\") `Or` (Var \"X\") `And` Var \"Y\"))"
  , "(fv ((Const True `And` Const False) `Or` Var \"X\"))"
  , "(fv ((Const True `And` Const False) `Or` Not (Var \"X\")))"
  , "(fv (Var \"Z\" `Or` Var \"X\" `And` (Not (Var \"W\" `Or` Var \"Y\"))))"
  , "(fv (Var \"Z\" `Or` Const True `And` (Not (Var \"W\" `Or` Var \"Y\"))))"
  , "(fv ((Const True `And` Const False) `Or` Const True))"
  , "(countOccurs \"X\" (Var \"Y\"))"
  , "(countOccurs \"X\" (Var \"X\"))"
  , "(countOccurs \"X\" (And (And (Var \"Y\") (Var \"Z\")) (Or (Var \"X\") (Var \"X\"))))"
  , "(countOccurs \"Z\" (And (And (Var \"Y\") (Var \"Z\")) (Or (Var \"X\") (Var \"X\"))))"
  , "(countOccurs \"A\" (And (And (Var \"Y\") (Var \"Z\")) (Or (Var \"X\") (Var \"X\"))))"
  , "(countOccurs \"X\" (Not (Not (Not (Var \"X\")))))"
  , "(countOccurs \"Z\" (Not (Not (Not (Var \"X\")))))"
  , "(countOccurs \"X\" (Or (And (Var \"X\") (Or (And (Var \"Z\") (Var \"Z\")) (Not (And (Var \"X\") (Var \"Y\"))))) (Not (Var \"Y\"))))"
  , "(countOccurs \"Y\" (Or (And (Var \"X\") (Or (And (Var \"Z\") (Var \"Z\")) (Not (And (Var \"X\") (Var \"Y\"))))) (Not (Var \"Y\"))))"
  , "(countOccurs \"Z\" (Or (And (Var \"X\") (Or (And (Var \"Z\") (Var \"Z\")) (Not (And (Var \"X\") (Var \"Y\"))))) (Not (Var \"Y\"))))"
  , "(setTrue \"X\" (Var \"Y\"))"
  , "(setTrue \"X\" (Var \"X\"))"
  , "(setTrue \"X\" (And (And (Var \"Y\") (Var \"Z\")) (Or (Var \"X\") (Var \"X\"))))"
  , "(setTrue \"Z\" (And (And (Var \"Y\") (Var \"Z\")) (Or (Var \"X\") (Var \"X\"))))"
  , "(setTrue \"A\" (And (And (Var \"Y\") (Var \"Z\")) (Or (Var \"X\") (Var \"X\"))))"
  , "(setTrue \"X\" (Not (Not (Not (Var \"X\")))))"
  , "(setTrue \"Z\" (Not (Not (Not (Var \"X\")))))"
  , "(setTrue \"X\" (Or (And (Var \"X\") (Or (And (Var \"Z\") (Var \"Z\")) (Not (And (Var \"X\") (Var \"Y\"))))) (Not (Var \"Y\"))))"
  , "(setTrue \"Y\" (Or (And (Var \"X\") (Or (And (Var \"Z\") (Var \"Z\")) (Not (And (Var \"X\") (Var \"Y\"))))) (Not (Var \"Y\"))))"
  , "(setTrue \"Z\" (Or (And (Var \"X\") (Or (And (Var \"Z\") (Var \"Z\")) (Not (And (Var \"X\") (Var \"Y\"))))) (Not (Var \"Y\"))))"
  , "(lookUp \"X\" [(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"X\" [(\"W\",False),(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"Y\" [(\"W\",False),(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"Z\" [(\"W\",False),(\"X\",True),(\"Y\",False),(\"Z\",False)])"
  , "(lookUp \"Z\" [(\"W\",False),(\"X\",True),(\"Y\",False),(\"Z\",True)])"
  , "(lookUp \"Y\" [(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"W\" [(\"W\",False),(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"W\" [(\"W\",True),(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"Z\" [(\"W\",False),(\"Z\",False),(\"X\",True),(\"Y\",False)])"
  , "(lookUp \"Z\" [(\"W\",False),(\"X\",True),(\"Z\",True),(\"Y\",False)])"
  , "(eval [(\"X\",False),(\"Y\",True) ] (Var \"X\" `And` Var \"Y\"))"
  , "(eval [(\"X\",False),(\"Y\",True) ] (Var \"X\" `Or` Var \"Y\"))"
  , "(eval [(\"X\",False),(\"Y\",True) ] (Not (Var \"X\") `Or` (Var \"Y\")))"
  , "(eval [(\"X\",True) ,(\"Y\",False)] (Not (Var \"X\") `Or` (Var \"Y\")))"
  , "(eval [(\"X\",True) ,(\"Y\",True) ] (Not (Var \"X\") `Or` Not (Var \"Y\")))"
  , "(eval [(\"X\",False),(\"Y\",True) ,(\"Z\",False)] (Var \"X\" `And` Var \"Y\"))"
  , "(eval [(\"X\",False),(\"Y\",True) ,(\"Z\",False)] (Var \"Z\" `Or` Var \"Y\"))"
  , "(eval [(\"X\",False),(\"Y\",True) ,(\"Z\",True)] (Var \"Z\"))"
  , "(eval [(\"X\",True) ,(\"Y\",False),(\"W\",False)] (Not (Var \"X\") `Or` (Var \"Y\" `And` Var \"W\")))"
  , "(eval [(\"X\",True) ,(\"Y\",True),(\"W\",False)] (Not (Var \"X\") `Or` Not (Var \"Y\" `And` Var \"W\")))"
  , "(evalList (Var \"X\" `And` Var \"Y\") [[(\"X\",False),(\"Y\",True)]])"
  , "(evalList (Var \"X\" `And` Var \"Y\") [[(\"X\",False),(\"Y\",True)],[(\"X\",True),(\"Y\",True)]])"
  , "(evalList (Var \"X\" `Or` Var \"Y\") [[(\"X\",False),(\"Y\",True)]])"
  , "(evalList (Var \"X\" `Or` Var \"Y\") [])"
  , "(evalList (Not (Var \"X\") `Or` Not (Var \"Y\")) [[(\"X\",False),(\"Y\",True)],[(\"X\",True),(\"Y\",True)]])"
  , "(evalList (Not (Var \"X\") `Or` Not (Var \"Y\")) [[(\"X\",True) ,(\"Y\",True)]])"
  , "(evalList (Const True) [[(\"X\",False),(\"Y\",False)],[(\"X\",True),(\"Y\",True)]])"
  , "(evalList (Const False) [[(\"X\",False),(\"Y\",False)],[(\"X\",True),(\"Y\",True)]])"
  , "(evalList (Var \"X\" `And` Var \"Y\" `And` Var \"Z\") [[(\"X\",False),(\"Y\",True),(\"Z\",False)],[(\"X\",True),(\"Y\",True),(\"Z\",False)]])"
  , "(evalList (Var \"X\" `And` Var \"Y\" `And` Var \"Z\") [[(\"X\",False),(\"Y\",True),(\"Z\",False)],[(\"X\",True),(\"Y\",True),(\"Z\",True)],[(\"X\",True),(\"Y\",True),(\"Z\",False)]])"
  , "(extendEnv [] \"X\")"
  , "(extendEnv [[(\"X\", True)]] \"\")"
  , "(extendEnv [] \"\")"
  , "(extendEnv [[(\"X\", False), (\"Y\", True)], []] \"Z\")"
  , "(extendEnv [[(\"X\", False), (\"Y\", True)], [(\"A\", False)]] \"Z\")"
  , "(extendEnv [[(\"D\", False)], [(\"B\", False), (\"T\", True)]] \"A\")"
  , "(extendEnv [[], [(\"X\", False), (\"C\", True), (\"A\", False)]] \"D\")"
  , "(extendEnv [[(\"X\", False), (\"C\", True), (\"A\", False)], []] \"D\")"
  , "(extendEnv [[(\"C\", False), (\"D\", True), (\"E\", False), (\"F\", False), (\"G\", True)], [(\"A\", False)]] \"B\")"
  , "(extendEnv [[(\"A\", False)], [(\"C\", True), (\"D\", False), (\"E\", True), (\"F\", False), (\"G\", False)]] \"B\")"
  , "(genEnvs [\"X\"])"
  , "(genEnvs [\"X\",\"Y\"])"
  , "(genEnvs [])"
  , "(genEnvs [\"X\",\"Y\",\"Z\"])"
  , "(genEnvs [\"X\",\"Y\",\"Z\",\"W\"])"
  , "(genEnvs (map (\\c -> c : \"\") ['a'..'e']))"
  , "(genEnvs [\"a\",\"b\"])"
  , "(genEnvs [\"a\",\"x\",\"b\",\"y\"])"
  , "(genEnvs [\"alpha\",\"gamma\"])"
  , "(genEnvs [\"v1\",\"v2\",\"v3\"])"
  , "(sat (Const True))"
  , "(sat (Var \"X\"))"
  , "(sat (Var \"X\" `And` Var \"Y\"))"
  , "(sat (Var \"X\" `And` (Not $ Var \"X\")))"
  , "(sat (Var \"X\" `Or` (Not $ Var \"X\")))"
  , "(sat (Var \"X\" `Or` (Var \"Y\" `And` Not (Var \"X\"))))"
  , "(sat (Var \"Y\" `And` (Not (Var \"Y\")) `Or` Var \"X\"))"
  , "(sat ((Not $ Var \"X\") `And` (Not $ Var \"Y\") `And` (Not $ Var \"Z\")))"
  , "(sat (Not (Var \"Y\" `And` Not (Var \"Y\"))))"
  , "(sat (Var \"X\" `And` Const False `Or` (Not (Var \"X\") `Or` Var \"Z\")))"
  , "(checkEq (Or (Var \"P\") (Var \"P\")) (Var \"P\"))"
  , "(checkEq (And (Var \"P\") (Var \"P\")) (Var \"P\"))"
  , "(checkEq (Or (Var \"P\") (Const True)) (Const True))"
  , "(checkEq (And (Var \"P\") (Const False)) (Const False))"
  , "(checkEq (Or (Var \"P\") (Not (Var \"P\"))) (Const False))"
  , "(checkEq (Not (And (Var \"P\") (Not (Var \"P\")))) (Const True))"
  , "(checkEq (Or (Var \"P\") (And (Var \"P\") (Var \"Q\"))) (Var \"P\"))"
  , "(checkEq (And (Var \"P\") (Or (Var \"P\") (Var \"Q\"))) (Var \"Q\"))"
  , "(checkEq (And (Var \"P\") (Or (Var \"Q\") (Var \"R\"))) (Or (And (Var \"P\") (Var \"Q\")) (And (Var \"P\") (Var \"R\"))))"
  , "(checkEq (Or (Var \"P\") (And (Var \"Q\") (Var \"R\"))) (And (Or (Var \"P\") (Var \"Q\")) (Or (Var \"P\") (Var \"R\"))))"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
