subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (flip elem ys) xs

eq :: (Eq a) => [a] -> [a] -> Bool
eq xs ys = subset xs ys && subset ys xs

tests =
  [ ((lexer "ifPositive (x) then (z) else (a - b)") == [IfPositiveK,LPar,VSym "x",RPar,ThenK,LPar,VSym "z",RPar,ElseK,LPar,VSym "a",SubOp,VSym "b",RPar])
  , ((lexer "ifPositive x\nthen\ny\nelse\nz") == [IfPositiveK,VSym "x",ThenK,VSym "y",ElseK,VSym "z"])
  , ((lexer "\\x.\\y.(ifPositive x then y else x)") == [Backslash,VSym "x",Dot,Backslash,VSym "y",Dot,LPar,IfPositiveK,VSym "x",ThenK,VSym "y",ElseK,VSym "x",RPar])
  , ((lexer "ifPositive 5 then Y (\\x.x) else 2") == [IfPositiveK,CSym 5,ThenK,YComb,LPar,Backslash,VSym "x",Dot,VSym "x",RPar,ElseK,CSym 2])
  , ((lexer "x y") == [VSym "x",VSym "y"])
  , ((lexer "(ifPositive (x - 10) then (Y (\\x.x)) else (ifPositive 20 then Y f else 1))") == [LPar,IfPositiveK,LPar,VSym "x",SubOp,CSym 10,RPar,ThenK,LPar,YComb,LPar,Backslash,VSym "x",Dot,VSym "x",RPar,RPar,ElseK,LPar,IfPositiveK,CSym 20,ThenK,YComb,VSym "f",ElseK,CSym 1,RPar,RPar])
  , ((lexer "f x y z t g h u i") == [VSym "f",VSym "x",VSym "y",VSym "z",VSym "t",VSym "g",VSym "h",VSym "u",VSym "i"])
  , ((lexer "ifPositive (x - 10) then (Y (\\x.x)) else (ifPositive 20 then Y f else 1) else (ifPositive 9 then 9 else 9)") == [IfPositiveK,LPar,VSym "x",SubOp,CSym 10,RPar,ThenK,LPar,YComb,LPar,Backslash,VSym "x",Dot,VSym "x",RPar,RPar,ElseK,LPar,IfPositiveK,CSym 20,ThenK,YComb,VSym "f",ElseK,CSym 1,RPar,ElseK,LPar,IfPositiveK,CSym 9,ThenK,CSym 9,ElseK,CSym 9,RPar])
  , ((lexer "x           y") == [VSym "x",VSym "y"])
  , ((lexer "123 ifPositive x then 456 then 3757499485858 then 939385 else Y 789") == [CSym 123,IfPositiveK,VSym "x",ThenK,CSym 456,ThenK,CSym 3757499485858,ThenK,CSym 939385,ElseK,YComb,CSym 789])
  , ((lexer "ifPositive 86 then 995 else \\x.\\y.\\z.(x y z)") == [IfPositiveK,CSym 86,ThenK,CSym 995,ElseK,Backslash,VSym "x",Dot,Backslash,VSym "y",Dot,Backslash,VSym "z",Dot,LPar,VSym "x",VSym "y",VSym "z",RPar])
  , ((lexer "(ifPositive (x - 10) then (Y (\\x.x)) else (ifPositive 20 then Y f else 1)) else (\\x.y)") == [LPar,IfPositiveK,LPar,VSym "x",SubOp,CSym 10,RPar,ThenK,LPar,YComb,LPar,Backslash,VSym "x",Dot,VSym "x",RPar,RPar,ElseK,LPar,IfPositiveK,CSym 20,ThenK,YComb,VSym "f",ElseK,CSym 1,RPar,RPar,ElseK,LPar,Backslash,VSym "x",Dot,VSym "y",RPar])
  , ((lexer "(ifPositive (x - 10) then (Y (\\x.x)) else (ifPositive 20 then Y f else 1)) else (\\x.y)") == [LPar,IfPositiveK,LPar,VSym "x",SubOp,CSym 10,RPar,ThenK,LPar,YComb,LPar,Backslash,VSym "x",Dot,VSym "x",RPar,RPar,ElseK,LPar,IfPositiveK,CSym 20,ThenK,YComb,VSym "f",ElseK,CSym 1,RPar,RPar,ElseK,LPar,Backslash,VSym "x",Dot,VSym "y",RPar])
  , ((parser [PT (Var "x"), PT (Var "y"), PT (App (Var "x") (Var "y"))]) == Left (App (App (Var "x") (Var "y")) (App (Var "x") (Var "y"))))
  , ((parser [IfPositiveK, VSym "s", ThenK, VSym "t", ElseK, VSym "u"]) == Left (IfPos (Var "s") (Var "t") (Var "u")))
  , ((parser [Backslash, VSym "x", Dot, VSym "y"]) == Left (Abs "x" (Var "y")))
  , ((parser [IfPositiveK, VSym "s", ThenK, IfPositiveK, VSym "t", ThenK, VSym "u", ElseK, VSym "v", ElseK, VSym "w"]) == Left (IfPos (Var "s") (IfPos (Var "t") (Var "u") (Var "v")) (Var "w")))
  , ((parser [IfPositiveK,LPar,VSym "x",RPar,ThenK,LPar,VSym "z",RPar,ElseK,LPar,VSym "a",SubOp,VSym "b",RPar]) == Left (IfPos (Var "x") (Var "z") (Sub (Var "a") (Var "b"))))
  , ((parser [IfPositiveK,CSym 5,ThenK,YComb,LPar,Backslash,VSym "x",Dot,VSym "x",RPar,ElseK,CSym 2]) == Left (IfPos (Num 5) (App Y (Abs "x" (Var "x"))) (Num 2)))
  , ((parser [VSym "x",VSym "y"]) == Left (App (Var "x") (Var "y")))
  , ((parser [CSym 123,IfPositiveK,VSym "x",ThenK,CSym 456,ElseK,YComb,CSym 789]) == Left (App (App (Num 123) (IfPos (Var "x") (Num 456) Y)) (Num 789)))
  , ((parser [CSym 3, SubOp, CSym 2]) == Left (Sub (Num 3) (Num 2)))
  , ((parser [PT (Abs "x" (Abs "y" (App (Var "x") (Var "y")))), PT (Abs "z" (Var "z"))]) == Left (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Abs "z" (Var "z"))))
  , ((tsubst ("a", Fun Ints (TVar "b")) (Fun (TVar "a") (TVar "c"))) == Fun (Fun Ints (TVar "b")) (TVar "c"))
  , ((tsubst ("c", Ints) (Fun (TVar "a") (Fun (TVar "b") (TVar "c")))) == Fun (TVar "a") (Fun (TVar "b") Ints))
  , ((tsubst ("a", Fun Ints Ints) (Fun (TVar "a") (TVar "a"))) == Fun (Fun Ints Ints) (Fun Ints Ints))
  , ((csubst ("a", Ints) (TVar "a", Ints)) == (Ints,Ints))
  , ((csubst ("a", Ints) (Fun (TVar "a") (TVar "b"), Fun (TVar "a") Ints)) == (Fun Ints (TVar "b"),Fun Ints Ints))
  , ((csubst ("z", Fun Ints Ints) (TVar "x", TVar "y")) == (TVar "x",TVar "y"))
  , ((getTVars (Fun (TVar "x") (TVar "y"))) `eq` ["x","y"])
  , ((getTVars (TVar "x")) `eq` ["x"])
  , ((getTVarsCxt [("x", Ints), ("y", TVar "a")]) `eq` ["a"])
  , ((getTVarsCxt [("z", Ints)]) `eq` [])
  , ((genConstrs [("x", Ints), ("y", Fun Ints Ints)] (Var "x") Ints) `eq` [(Ints,Ints)])
  , ((genConstrs [("x", Ints)] (Var "x") (TVar "a")) `eq` [(TVar "a",Ints)])
  , ((genConstrs [] (App (Abs "x" (Var "x")) (Num 10)) Ints) `eq` [(Fun (TVar "a") Ints,Fun (TVar "b") (TVar "c")),(TVar "c",TVar "b"),(TVar "a",Ints)])
  , ((genConstrs [] (IfPos (Num 5) (Num 10) (Num 20)) Ints) `eq` [(Ints,Ints),(Ints,Ints),(Ints,Ints)])
  , ((genConstrs [] (App (Abs "x" (Var "x")) (Abs "y" (Var "y"))) (TVar "b")) `eq` [(Fun (TVar "c") (TVar "b"),Fun (TVar "d") (TVar "e")),(TVar "e",TVar "d"),(TVar "c",Fun (TVar "d") (TVar "e")),(TVar "e",TVar "d")])
  , ((genConstrs [("y", Ints)] (App (Abs "x" (Var "x")) (Var "y")) (TVar "a")) `eq` [(Fun (TVar "b") (TVar "a"),Fun (TVar "c") (TVar "d")),(TVar "d",TVar "c"),(TVar "b",Ints)])
  , ((genConstrs [("x", TVar "a")] (Var "x") (TVar "a")) `eq` [(TVar "a",TVar "a")])
  , ((genConstrs [("x", TVar "d"), ("y", TVar "c")] (App (Var "x") (Var "y")) (TVar "a")) `eq` [(Fun (TVar "e") (TVar "a"),TVar "d"),(TVar "e",TVar "c")])
  , ((genConstrs [("z", TVar "a")] (App (Abs "x" (Var "x")) (Var "z")) (TVar "b")) `eq` [(Fun (TVar "c") (TVar "b"),Fun (TVar "d") (TVar "e")),(TVar "e",TVar "d"),(TVar "c",TVar "a")])
  , ((unify [(Fun (TVar "b") (TVar "a"),Fun (TVar "c") (TVar "d")),(TVar "d",TVar "c"),(TVar "b",Ints)]) `eq` [("b",TVar "c"),("a",TVar "d"),("d",TVar "c"),("c",Ints)])
  , ((unify [(TVar "a",Ints)]) `eq` [("a",Ints)])
  , ((unify [(Fun (TVar "c") (Fun (TVar "b") (TVar "a")),Fun (TVar "d") (TVar "e")),(TVar "e",Fun (TVar "f") (TVar "g")),(TVar "g",TVar "d"),(TVar "c",Ints),(TVar "b",Ints)]) `eq` [("c",TVar "d"),("e",Fun (TVar "b") (TVar "a")),("b",TVar "f"),("a",TVar "g"),("g",TVar "d"),("d",Ints),("f",Ints)])
  , ((unify [(Ints,Ints),(Fun (TVar "b") (TVar "a"),Fun (TVar "c") (TVar "d")),(TVar "d",TVar "c"),(TVar "b",Ints),(TVar "a",Ints)]) `eq` [("b",TVar "c"),("a",TVar "d"),("d",TVar "c"),("c",Ints)])
  , ((unify [(TVar "a",Fun (TVar "b") (TVar "c")),(TVar "c",Fun (TVar "d") (TVar "e")),(TVar "e",TVar "b")]) `eq` [("a",Fun (TVar "b") (TVar "c")),("c",Fun (TVar "d") (TVar "e")),("e",TVar "b")])
  , ((unify [(TVar "a",Fun (TVar "b") (TVar "c")),(TVar "c",Fun (TVar "d") (TVar "e")),(TVar "e",Fun (TVar "f") (TVar "g")),(Fun (TVar "h") (TVar "g"),TVar "b"),(Fun (TVar "i") (TVar "h"),TVar "d"),(TVar "i",TVar "f")]) `eq` [("a",Fun (TVar "b") (TVar "c")),("c",Fun (TVar "d") (TVar "e")),("e",Fun (TVar "f") (TVar "g")),("b",Fun (TVar "h") (TVar "g")),("d",Fun (TVar "i") (TVar "h")),("i",TVar "f")])
  , ((unify [(Ints, Ints)]) `eq` [])
  , ((unify [(TVar "a", TVar "b")]) `eq` [("a",TVar "b")])
  , ((unify [(Fun (TVar "a") (TVar "b"), TVar "c")]) `eq` [("c",Fun (TVar "a") (TVar "b"))])
  , ((unify [(Fun (TVar "a") (TVar "b"), Fun (TVar "d") (TVar "c")), (TVar "a", TVar "c")]) `eq` [("a",TVar "d"),("b",TVar "c"),("d",TVar "c")])
  , ((infer (Num 5)) == Ints)
  , ((infer (App (Abs "x" (Var "x")) (Num 5))) == Ints)
  , ((infer (App (App (Abs "x" (Abs "y" (Var "x"))) (Num 5)) (Num 6))) == Ints)
  , ((infer (IfPos (Num 1) (App (Abs "x" (Var "x")) (Num 5)) (Num 6))) == Ints)
  , ((infer (Sub (Sub (Num 10) (Num 3)) (Num 2))) == Ints)
  , ((infer (Abs "x" (Var "x"))) == Fun (TVar "b") (TVar "b"))
  , ((infer (Abs "x" (Abs "y" (Abs "z" (App (Var "y") (App (Var "x") (Var "z"))))))) == Fun (Fun (TVar "f") (TVar "h")) (Fun (Fun (TVar "h") (TVar "g")) (Fun (TVar "f") (TVar "g"))))
  , ((infer (Abs "x" (Sub (Num 10) (Var "x")))) == Fun Ints Ints)
  , ((infer (Abs "x" (IfPos (Var "x") (Var "x") (Var "x")))) == Fun Ints Ints)
  , ((infer (Abs "x" (Abs "y" (Var "x")))) == Fun (TVar "b") (Fun (TVar "d") (TVar "b")))
  , ((subst ("x", Num 42) (App Y (Abs "y" (App (Var "x") (Var "y"))))) == App Y (Abs "y" (App (Num 42) (Var "y"))))
  , ((subst ("x", Num 42) (Sub (Num 1) (Var "x"))) == Sub (Num 1) (Num 42))
  , ((subst ("x", Num 42) (IfPos (IfPos (Var "x") (Var "y") (Var "z")) (Var "w") (Var "z"))) == IfPos (IfPos (Num 42) (Var "y") (Var "z")) (Var "w") (Var "z"))
  , ((subst ("x", Num 42) (Sub (Sub (Sub (Var "x") (Var "y")) (Var "z")) (Sub (Var "x") (Var "y")))) == Sub (Sub (Sub (Num 42) (Var "y")) (Var "z")) (Sub (Num 42) (Var "y")))
  , ((subst ("x", App (Var "y") (Num 5)) (Sub (Var "x") (Num 10))) == Sub (App (Var "y") (Num 5)) (Num 10))
  , ((subst ("p", IfPos (Var "s") (Var "t") (Var "u")) ( IfPos (Var "p") (Var "q") (Var "r"))) == IfPos (IfPos (Var "s") (Var "t") (Var "u")) (Var "q") (Var "r"))
  , ((subst ("x", App (Var "a") (App (Var "b") (Var "c"))) (App (Var "x") (Var "y"))) == App (App (Var "a") (App (Var "b") (Var "c"))) (Var "y"))
  , ((subst ("y", Abs "z" (Var "z")) (Abs "x" (App (Var "x") (Var "y")))) == Abs "x" (App (Var "x") (Abs "z" (Var "z"))))
  , ((subst ("x", Num 5) (Abs "y" (Sub (Var "y") (Var "x")))) == Abs "y" (Sub (Var "y") (Num 5)))
  , ((subst ("z", App (Var "x") (Var "y")) (Var "z")) == App (Var "x") (Var "y"))
  , ((red (Num 5)) == Num 5)
  , ((red (App (Abs "x" (Var "x")) (Num 5))) == Num 5)
  , ((red (App (App (Abs "x" (Abs "y" (Var "x"))) (Num 5)) (Num 6))) == App (Abs "y" (Num 5)) (Num 6))
  , ((red (IfPos (Num 1) (App (Abs "x" (Var "x")) (Num 5)) (Num 6))) == Num 5)
  , ((red (Sub (Sub (Num 10) (Num 3)) (Num 2))) == Sub (Num 7) (Num 2))
  , ((red (App (Var "f") (Var "x"))) == App (Var "f") (Var "x"))
  , ((red (Abs "x" (App (Var "x") (Var "x")))) == Abs "x" (App (Var "x") (Var "x")))
  , ((red (Abs "x" (Abs "y" (Var "x")))) == Abs "x" (Abs "y" (Var "x")))
  , ((red (App (App Y (App (App (App (Abs "f" (Abs "n" (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))))) (Var "n")) (Num 0)) (App (App (App (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))) (Sub (Var "n") (Num 1))) (Num 1)) (Sub (App (Var "f") (Sub (Var "n") (Num 1))) (Sub (Num 0) (App (Var "f") (Sub (Var "n") (Num 2)))))))) (Num 10))) == App (App (App (App (App (Abs "f" (Abs "n" (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))))) (Var "n")) (Num 0)) (App (App (App (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))) (Sub (Var "n") (Num 1))) (Num 1)) (Sub (App (Var "f") (Sub (Var "n") (Num 1))) (Sub (Num 0) (App (Var "f") (Sub (Var "n") (Num 2))))))) (App Y (App (App (App (Abs "f" (Abs "n" (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))))) (Var "n")) (Num 0)) (App (App (App (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))) (Sub (Var "n") (Num 1))) (Num 1)) (Sub (App (Var "f") (Sub (Var "n") (Num 1))) (Sub (Num 0) (App (Var "f") (Sub (Var "n") (Num 2))))))))) (Num 10))
  , ((red (App Y (App (App (App (Abs "f" (Abs "n" (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))))) (Var "n")) (Num 0)) (App (App (App (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))) (Sub (Var "n") (Num 1))) (Num 1)) (Sub (App (Var "f") (Sub (Var "n") (Num 1))) (Sub (Num 0) (App (Var "f") (Sub (Var "n") (Num 2))))))))) == App (App (App (App (Abs "f" (Abs "n" (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))))) (Var "n")) (Num 0)) (App (App (App (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))) (Sub (Var "n") (Num 1))) (Num 1)) (Sub (App (Var "f") (Sub (Var "n") (Num 1))) (Sub (Num 0) (App (Var "f") (Sub (Var "n") (Num 2))))))) (App Y (App (App (App (Abs "f" (Abs "n" (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))))) (Var "n")) (Num 0)) (App (App (App (Abs "x" (Abs "y" (Abs "z" (IfPos (Var "x") (Var "z") (IfPos (Sub (Num 0) (Var "x")) (Var "z") (Var "y")))))) (Sub (Var "n") (Num 1))) (Num 1)) (Sub (App (Var "f") (Sub (Var "n") (Num 1))) (Sub (Num 0) (App (Var "f") (Sub (Var "n") (Num 2)))))))))
  , ((reds (Num 5)) == Num 5)
  , ((reds (App (Abs "x" (Var "x")) (Num 5))) == Num 5)
  , ((reds (App (App (Abs "x" (Abs "y" (Var "x"))) (Num 5)) (Num 6))) == Num 5)
  , ((reds (IfPos (Num 1) (App (Abs "x" (Var "x")) (Num 5)) (Num 6))) == Num 5)
  , ((reds (Sub (Sub (Num 10) (Num 3)) (Num 2))) == Num 5)
  , ((reds (App (Var "f") (Var "x"))) == App (Var "f") (Var "x"))
  , ((reds (Abs "x" (App (Var "x") (Var "x")))) == Abs "x" (App (Var "x") (Var "x")))
  , ((reds (Abs "x" (Abs "y" (Var "x")))) == Abs "x" (Abs "y" (Var "x")))
  ]
testLinesString =
  [ "(lexer \"ifPositive (x) then (z) else (a - b)\")"
  , "(lexer \"ifPositive x\\nthen\\ny\\nelse\\nz\")"
  , "(lexer \"\\\\x.\\\\y.(ifPositive x then y else x)\")"
  , "(lexer \"ifPositive 5 then Y (\\\\x.x) else 2\")"
  , "(lexer \"x y\")"
  , "(lexer \"(ifPositive (x - 10) then (Y (\\\\x.x)) else (ifPositive 20 then Y f else 1))\")"
  , "(lexer \"f x y z t g h u i\")"
  , "(lexer \"ifPositive (x - 10) then (Y (\\\\x.x)) else (ifPositive 20 then Y f else 1) else (ifPositive 9 then 9 else 9)\")"
  , "(lexer \"x           y\")"
  , "(lexer \"123 ifPositive x then 456 then 3757499485858 then 939385 else Y 789\")"
  , "(lexer \"ifPositive 86 then 995 else \\\\x.\\\\y.\\\\z.(x y z)\")"
  , "(lexer \"(ifPositive (x - 10) then (Y (\\\\x.x)) else (ifPositive 20 then Y f else 1)) else (\\\\x.y)\")"
  , "(lexer \"(ifPositive (x - 10) then (Y (\\\\x.x)) else (ifPositive 20 then Y f else 1)) else (\\\\x.y)\")"
  , "(parser [PT (Var \"x\"), PT (Var \"y\"), PT (App (Var \"x\") (Var \"y\"))])"
  , "(parser [IfPositiveK, VSym \"s\", ThenK, VSym \"t\", ElseK, VSym \"u\"])"
  , "(parser [Backslash, VSym \"x\", Dot, VSym \"y\"])"
  , "(parser [IfPositiveK, VSym \"s\", ThenK, IfPositiveK, VSym \"t\", ThenK, VSym \"u\", ElseK, VSym \"v\", ElseK, VSym \"w\"])"
  , "(parser [IfPositiveK,LPar,VSym \"x\",RPar,ThenK,LPar,VSym \"z\",RPar,ElseK,LPar,VSym \"a\",SubOp,VSym \"b\",RPar])"
  , "(parser [IfPositiveK,CSym 5,ThenK,YComb,LPar,Backslash,VSym \"x\",Dot,VSym \"x\",RPar,ElseK,CSym 2])"
  , "(parser [VSym \"x\",VSym \"y\"])"
  , "(parser [CSym 123,IfPositiveK,VSym \"x\",ThenK,CSym 456,ElseK,YComb,CSym 789])"
  , "(parser [CSym 3, SubOp, CSym 2])"
  , "(parser [PT (Abs \"x\" (Abs \"y\" (App (Var \"x\") (Var \"y\")))), PT (Abs \"z\" (Var \"z\"))])"
  , "(tsubst (\"a\", Fun Ints (TVar \"b\")) (Fun (TVar \"a\") (TVar \"c\")))"
  , "(tsubst (\"c\", Ints) (Fun (TVar \"a\") (Fun (TVar \"b\") (TVar \"c\"))))"
  , "(tsubst (\"a\", Fun Ints Ints) (Fun (TVar \"a\") (TVar \"a\")))"
  , "(csubst (\"a\", Ints) [(TVar \"a\", Ints), (Fun (TVar \"b\") Ints, TVar \"c\")])"
  , "(csubst (\"a\", Ints)[(Fun (TVar \"a\") (TVar \"b\"), Fun Ints Ints), (TVar \"c\", Fun (TVar \"a\") Ints)])"
  , "(csubst (\"z\", Fun Ints Ints) [(TVar \"x\", TVar \"y\"), (Fun (TVar \"z\") Ints, Fun Ints (TVar \"x\"))])"
  , "(getTVars (Fun (TVar \"x\") (TVar \"y\")))"
  , "(getTVars (TVar \"x\"))"
  , "(getTVarsCxt [(\"x\", Ints), (\"y\", TVar \"a\")])"
  , "(getTVarsCxt [(\"z\", Ints)])"
  , "(genConstrs [(\"x\", Ints), (\"y\", Fun Ints Ints)] (Var \"x\") Ints)"
  , "(genConstrs [(\"x\", Ints)] (Var \"x\") (TVar \"a\"))"
  , "(genConstrs [] (App (Abs \"x\" (Var \"x\")) (Num 10)) Ints)"
  , "(genConstrs [] (IfPos (Num 5) (Num 10) (Num 20)) Ints)"
  , "(genConstrs [] (App (Abs \"x\" (Var \"x\")) (Abs \"y\" (Var \"y\"))) (TVar \"b\"))"
  , "(genConstrs [(\"y\", Ints)] (App (Abs \"x\" (Var \"x\")) (Var \"y\")) (TVar \"a\"))"
  , "(genConstrs [(\"x\", TVar \"a\")] (Var \"x\") (TVar \"a\"))"
  , "(genConstrs [(\"x\", TVar \"d\"), (\"y\", TVar \"c\")] (App (Var \"x\") (Var \"y\")) (TVar \"a\"))"
  , "(genConstrs [(\"z\", TVar \"a\")] (App (Abs \"x\" (Var \"x\")) (Var \"z\")) (TVar \"b\"))"
  , "(unify [(Fun (TVar \"b\") (TVar \"a\"),Fun (TVar \"c\") (TVar \"d\")),(TVar \"d\",TVar \"c\"),(TVar \"b\",Ints)])"
  , "(unify [(TVar \"a\",Ints)])"
  , "(unify [(Fun (TVar \"c\") (Fun (TVar \"b\") (TVar \"a\")),Fun (TVar \"d\") (TVar \"e\")),(TVar \"e\",Fun (TVar \"f\") (TVar \"g\")),(TVar \"g\",TVar \"d\"),(TVar \"c\",Ints),(TVar \"b\",Ints)])"
  , "(unify [(Ints,Ints),(Fun (TVar \"b\") (TVar \"a\"),Fun (TVar \"c\") (TVar \"d\")),(TVar \"d\",TVar \"c\"),(TVar \"b\",Ints),(TVar \"a\",Ints)])"
  , "(unify [(TVar \"a\",Fun (TVar \"b\") (TVar \"c\")),(TVar \"c\",Fun (TVar \"d\") (TVar \"e\")),(TVar \"e\",TVar \"b\")])"
  , "(unify [(TVar \"a\",Fun (TVar \"b\") (TVar \"c\")),(TVar \"c\",Fun (TVar \"d\") (TVar \"e\")),(TVar \"e\",Fun (TVar \"f\") (TVar \"g\")),(Fun (TVar \"h\") (TVar \"g\"),TVar \"b\"),(Fun (TVar \"i\") (TVar \"h\"),TVar \"d\"),(TVar \"i\",TVar \"f\")])"
  , "(unify [(Ints, Ints)])"
  , "(unify [(TVar \"a\", TVar \"b\")])"
  , "(unify [(Fun (TVar \"a\") (TVar \"b\"), TVar \"c\")])"
  , "(unify [(Fun (TVar \"a\") (TVar \"b\"), Fun (TVar \"d\") (TVar \"c\")), (TVar \"a\", TVar \"c\")])"
  , "(infer (Num 5))"
  , "(infer (App (Abs \"x\" (Var \"x\")) (Num 5)))"
  , "(infer (App (App (Abs \"x\" (Abs \"y\" (Var \"x\"))) (Num 5)) (Num 6)))"
  , "(infer (IfPos (Num 1) (App (Abs \"x\" (Var \"x\")) (Num 5)) (Num 6)))"
  , "(infer (Sub (Sub (Num 10) (Num 3)) (Num 2)))"
  , "(infer (Abs \"x\" (Var \"x\")))"
  , "(infer (Abs \"x\" (Abs \"y\" (Abs \"z\" (App (Var \"y\") (App (Var \"x\") (Var \"z\")))))))"
  , "(infer (Abs \"x\" (Sub (Num 10) (Var \"x\"))))"
  , "(infer (Abs \"x\" (IfPos (Var \"x\") (Var \"x\") (Var \"x\"))))"
  , "(infer (Abs \"x\" (Abs \"y\" (Var \"x\"))))"
  , "(subst (\"x\", Num 42) (App Y (Abs \"y\" (App (Var \"x\") (Var \"y\")))))"
  , "(subst (\"x\", Num 42) (Sub (Num 1) (Var \"x\")))"
  , "(subst (\"x\", Num 42) (IfPos (IfPos (Var \"x\") (Var \"y\") (Var \"z\")) (Var \"w\") (Var \"z\")))"
  , "(subst (\"x\", Num 42) (Sub (Sub (Sub (Var \"x\") (Var \"y\")) (Var \"z\")) (Sub (Var \"x\") (Var \"y\"))))"
  , "(subst (\"x\", App (Var \"y\") (Num 5)) (Sub (Var \"x\") (Num 10)))"
  , "(subst (\"p\", IfPos (Var \"s\") (Var \"t\") (Var \"u\")) ( IfPos (Var \"p\") (Var \"q\") (Var \"r\")))"
  , "(subst (\"x\", App (Var \"a\") (App (Var \"b\") (Var \"c\"))) (App (Var \"x\") (Var \"y\")))"
  , "(subst (\"y\", Abs \"z\" (Var \"z\")) (Abs \"x\" (App (Var \"x\") (Var \"y\"))))"
  , "(subst (\"x\", Num 5) (Abs \"y\" (Sub (Var \"y\") (Var \"x\"))))"
  , "(subst (\"z\", App (Var \"x\") (Var \"y\")) (Var \"z\"))"
  , "(red (Num 5))"
  , "(red (App (Abs \"x\" (Var \"x\")) (Num 5)))"
  , "(red (App (App (Abs \"x\" (Abs \"y\" (Var \"x\"))) (Num 5)) (Num 6)))"
  , "(red (IfPos (Num 1) (App (Abs \"x\" (Var \"x\")) (Num 5)) (Num 6)))"
  , "(red (Sub (Sub (Num 10) (Num 3)) (Num 2)))"
  , "(red (App (Var \"f\") (Var \"x\")))"
  , "(red (Abs \"x\" (App (Var \"x\") (Var \"x\"))))"
  , "(red (Abs \"x\" (Abs \"y\" (Var \"x\"))))"
  , "(red (App (App Y (App (App (App (Abs \"f\" (Abs \"n\" (Abs \"x\" (Abs \"y\" (Abs \"z\" (IfPos (Var \"x\") (Var \"z\") (IfPos (Sub (Num 0) (Var \"x\")) (Var \"z\") (Var \"y\")))))))) (Var \"n\")) (Num 0)) (App (App (App (Abs \"x\" (Abs \"y\" (Abs \"z\" (IfPos (Var \"x\") (Var \"z\") (IfPos (Sub (Num 0) (Var \"x\")) (Var \"z\") (Var \"y\")))))) (Sub (Var \"n\") (Num 1))) (Num 1)) (Sub (App (Var \"f\") (Sub (Var \"n\") (Num 1))) (Sub (Num 0) (App (Var \"f\") (Sub (Var \"n\") (Num 2)))))))) (Num 10)))"
  , "(red (App Y (App (App (App (Abs \"f\" (Abs \"n\" (Abs \"x\" (Abs \"y\" (Abs \"z\" (IfPos (Var \"x\") (Var \"z\") (IfPos (Sub (Num 0) (Var \"x\")) (Var \"z\") (Var \"y\")))))))) (Var \"n\")) (Num 0)) (App (App (App (Abs \"x\" (Abs \"y\" (Abs \"z\" (IfPos (Var \"x\") (Var \"z\") (IfPos (Sub (Num 0) (Var \"x\")) (Var \"z\") (Var \"y\")))))) (Sub (Var \"n\") (Num 1))) (Num 1)) (Sub (App (Var \"f\") (Sub (Var \"n\") (Num 1))) (Sub (Num 0) (App (Var \"f\") (Sub (Var \"n\") (Num 2)))))))))"
  , "(reds (Num 5))"
  , "(reds (App (Abs \"x\" (Var \"x\")) (Num 5)))"
  , "(reds (App (App (Abs \"x\" (Abs \"y\" (Var \"x\"))) (Num 5)) (Num 6)))"
  , "(reds (IfPos (Num 1) (App (Abs \"x\" (Var \"x\")) (Num 5)) (Num 6)))"
  , "(reds (Sub (Sub (Num 10) (Num 3)) (Num 2)))"
  , "(reds (App (Var \"f\") (Var \"x\")))"
  , "(reds (Abs \"x\" (App (Var \"x\") (Var \"x\"))))"
  , "(reds (Abs \"x\" (Abs \"y\" (Var \"x\"))))"
  , "(reds (App (App Y (App (App (App (Abs \"f\" (Abs \"n\" (Abs \"x\" (Abs \"y\" (Abs \"z\" (IfPos (Var \"x\") (Var \"z\") (IfPos (Sub (Num 0) (Var \"x\")) (Var \"z\") (Var \"y\")))))))) (Var \"n\")) (Num 0)) (App (App (App (Abs \"x\" (Abs \"y\" (Abs \"z\" (IfPos (Var \"x\") (Var \"z\") (IfPos (Sub (Num 0) (Var \"x\")) (Var \"z\") (Var \"y\")))))) (Sub (Var \"n\") (Num 1))) (Num 1)) (Sub (App (Var \"f\") (Sub (Var \"n\") (Num 1))) (Sub (Num 0) (App (Var \"f\") (Sub (Var \"n\") (Num 2)))))))) (Num 10)))"
  , "(reds (App (Abs \"x\" (Var \"x\")) (Abs \"y\" (Var \"y\"))))"
  ]

main = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
