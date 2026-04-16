
tests =
  [ ((lexer "v * v <= 4 || tt && ff") == [VSym "v",BOp MulOp,VSym "v",BOp LteOp,CSym 4,BOp OrOp,BSym True,BOp AndOp,BSym False])
  , ((lexer "v := 5 - v") == [VSym "v",AssignOp,CSym 5,BOp SubOp,VSym "v"])
  , ((lexer "v + 5; nop") == [VSym "v",BOp AddOp,CSym 5,Semi,Keyword NopK])
  , ((lexer "c := ff; while ! (if c then tt else ff)") == [VSym "c",AssignOp,BSym False,Semi,Keyword WhileK,NotOp,LPar,Keyword IfK,VSym "c",Keyword ThenK,BSym True,Keyword ElseK,BSym False,RPar])
  , ((lexer "if c then d; v := v + 5") == [Keyword IfK,VSym "c",Keyword ThenK,VSym "d",Semi,VSym "v",AssignOp,VSym "v",BOp AddOp,CSym 5])
  , ((lexer "if v%6 == 0 then v := 5 else v := 6") == [Keyword IfK,VSym "v",BOp ModOp,CSym 6,BOp EqOp,CSym 0,Keyword ThenK,VSym "v",AssignOp,CSym 5,Keyword ElseK,VSym "v",AssignOp,CSym 6])
  , ((lexer "` == !=") == [Err "` == !="])
  , ((lexer "if v%6 == 0 then v else ()") == [Keyword IfK,VSym "v",BOp ModOp,CSym 6,BOp EqOp,CSym 0,Keyword ThenK,VSym "v",Keyword ElseK,LPar,RPar])
  , ((lexer "return v / 2 ^ 3") == [Keyword ReturnK,VSym "v",BOp DivOp,CSym 2,BOp ExpOp,CSym 3])
  , ((lexer "tt < 6 >= 4") == [BSym True,BOp LtOp,CSym 6,BOp GteOp,CSym 4])
  , ((sr [] [VSym "x",AssignOp,VSym "x",BOp MulOp,VSym "x",Semi]) == [PI (Assign "x" (Mul (Var "x") (Var "x")))])
  , ((sr [] [VSym "x",AssignOp,VSym "x",BOp ExpOp,VSym "x",Semi]) == [PI (Assign "x" (Exp (Var "x") (Var "x")))])
  , ((sr [] [VSym "x",AssignOp,VSym "x",BOp ModOp,VSym "x",Semi]) == [PI (Assign "x" (Mod (Var "x") (Var "x")))])
  , ((sr [] [VSym "x",AssignOp,VSym "x",BOp DivOp,VSym "x",Semi]) == [PI (Assign "x" (Div (Var "x") (Var "x")))])
  , ((sr [] [VSym "v",AssignOp,CSym 5,BOp AddOp,VSym "x",Semi]) == [PI (Assign "v" (Add (Num 5) (Var "x")))])
  , ((sr [] [Keyword IfK,LPar,VSym "x",BOp SubOp,CSym 2,BOp GtOp,CSym 4,RPar,Keyword ThenK,Keyword ReturnK,CSym 4,Semi,Keyword ElseK,Keyword ReturnK,VSym "x",Semi]) == [PI (IfThenElse (Gt (Sub (Var "x") (Num 2)) (Num 4)) (Return (Num 4)) (Return (Var "x")))])
  , ((sr [] [Keyword IfK,BSym True,Keyword ThenK,Keyword ReturnK,CSym 5,Semi,Keyword ElseK,Keyword ReturnK,CSym 4,Semi]) == [PI (IfThenElse (Const True) (Return (Num 5)) (Return (Num 4)))])
  , ((sr [] [Keyword IfK,NotOp,LPar,BSym False,BOp OrOp,LPar,BSym True,BOp AndOp,BSym True,RPar,RPar,Keyword ThenK,Keyword ReturnK,CSym 2,Semi]) == [PI (IfThen (Not (Or (Const False) (And (Const True) (Const True)))) (Return (Num 2)))])
  , ((sr [] [Keyword IfK,LPar,VSym "x",BOp EqOp,CSym 5,BOp OrOp,VSym "x",BOp LteOp,CSym 5,BOp OrOp,VSym "x",BOp GteOp,CSym 5,BOp OrOp,VSym "x",BOp LtOp,CSym 5,BOp OrOp,VSym "x",BOp GtOp,CSym 5,RPar,Keyword ThenK,Keyword ReturnK,CSym 6,Semi]) == [PI (IfThen (Or (Or (Or (Or (Eq (Var "x") (Num 5)) (Lte (Var "x") (Num 5))) (Gte (Var "x") (Num 5))) (Lt (Var "x") (Num 5))) (Gt (Var "x") (Num 5))) (Return (Num 6)))])
  , ((sr [] [Keyword WhileK,LPar,VSym "x",BOp NeqOp,CSym 5,RPar,LBra,Keyword ReturnK,CSym 6,Semi,Keyword ReturnK,CSym 7,Semi,RBra]) == [PI (While (Neq (Var "x") (Num 5)) (Do [Return (Num 6),Return (Num 7)]))])
  , ((evala [("x", 5), ("y", 4)] (Add (Var "x") (Div (Var "y") (Num 2)))) == 7)
  , ((evala [("x", 5)] (Div (Mod (Div (Var "x") (Num 3)) (Num 6)) (Num 3))) == 0)
  , ((evala [("x", 4)] (Sub (Add (Mul (Var "x") (Num 3)) (Num 3)) (Mod (Num 3) (Exp (Num 6) (Num 3))))) == 12)
  , ((evala [] (Sub (Num 3) (Num 1))) == 2)
  , ((evala [] (Div (Num 3) (Num 1))) == 3)
  , ((evalb [("x", 4)] (And (Const True) (Lt (Var "x") (Num 3)))) == False)
  , ((evalb [("x", 6)] (Or (Gte (Var "x") (Num 2)) (Lte (Var "x") (Num 3)))) == True)
  , ((evalb [("x", 2)] (Not (Neq (Var "x") (Num 2)))) == True)
  , ((evalb [("x", 2)] (Eq (Var "x") (Num 2))) == True)
  , ((evalb [("x", 3)] (And (Gt (Var "x") (Num 2)) (Const False))) == False)
  , ((execList (reverse [(IfThen (Eq (Mod (Var "n") (Num 2)) (Num 0)) (Return (Num 0))), (Assign "n" (Num 91))]) []) == [("n",91)])
  , ((execList (reverse [(While (Lt (Var "i") (Var "n")) (Do [IfThen (Eq (Mod (Var "n") (Var "i")) (Num 0)) (Return (Num 0)),Assign "i" (Add (Var "i") (Num 2))]))]) [("n",91), ("i",3)]) == [("",0)])
  , ((execList (reverse [(While (Lt (Var "i") (Var "n")) (Do [IfThen (Eq (Mod (Var "n") (Var "i")) (Num 0)) (Return (Num 0)),Assign "i" (Add (Var "i") (Num 2))]))]) [("n",97), ("i",3)]) == [("n",97),("i",97)])
  , ((execList (reverse [(Return (Var "z")), (Assign "z" (Add (Mul (Num 2) (Var "x")) (Mod (Var "y") (Exp (Num 3) (Num 2))))), (Assign "y" (Num 10)), (Assign "x" (Num 5))]) []) == [("",11)])
  , ((execList (reverse [(Return (Var "sum")), (While (Lt (Var "c") (Var "max")) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "sum" (Add (Var "sum") (Var "c"))])), (Assign "sum" (Num 0)), (Assign "max" (Num 100)), (Assign "c" (Num 0))]) []) == [("",5050)])
  , ((execList (reverse [(Return (Var "x")), (While (Lte (Var "c") (Sub (Var "fib") (Num 1))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "z" (Add (Var "x") (Var "y")),Assign "x" (Var "y"),Assign "y" (Var "z")])), (Assign "c" (Num 0)), (Assign "y" (Num 1)), (Assign "x" (Num 0)), (Assign "fib" (Num 8))]) []) == [("",21)])
  , ((execList (reverse [(Return (Var "acc")), (While (Not (Lte (Var "fact") (Var "c"))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))])), (Assign "c" (Num 1)), (Assign "acc" (Num 1)), (Assign "fact" (Num 5))]) []) == [("",120)])
  , ((execList (reverse [(Return (Var "sum")), (While (Lt (Var "i") (Var "x")) (Do [Assign "sum" (Add (Var "sum") (Var "y")),Assign "i" (Add (Var "i") (Num 1))])), (Assign "i" (Num 0)), (Assign "sum" (Num 0)), (Assign "y" (Num 7)), (Assign "x" (Num 6))]) []) == [("",42)])
  , ((execList (reverse [(Return (Var "acc")), (While (Not (Lte (Var "fact") (Var "c"))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))])), (Assign "c" (Num 1)), (Assign "acc" (Num 1))]) [("fact", 8)]) == [("",40320)])
  , ((execList (reverse [(Return (Var "x")), (While (Lte (Var "c") (Sub (Var "fib") (Num 1))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "z" (Add (Var "x") (Var "y")),Assign "x" (Var "y"),Assign "y" (Var "z")])), (Assign "c" (Num 0)), (Assign "y" (Num 1)), (Assign "x" (Num 0))]) [("fib", 9)]) == [("",34)])
  ]
testLinesString =
  [ "(lexer \"v * v <= 4 || tt && ff\")"
  , "(lexer \"v := 5 - v\")"
  , "(lexer \"v + 5; nop\")"
  , "(lexer \"c := ff; while ! (if c then tt else ff)\")"
  , "(lexer \"if c then d; v := v + 5\")"
  , "(lexer \"if v%6 == 0 then v := 5 else v := 6\")"
  , "(lexer \"` == !=\")"
  , "(lexer \"if v%6 == 0 then v else ()\")"
  , "(lexer \"return v / 2 ^ 3\")"
  , "(lexer \"tt < 6 >= 4\")"
  , "(sr [] [VSym \"x\",AssignOp,VSym \"x\",BOp MulOp,VSym \"x\",Semi])"
  , "(sr [] [VSym \"x\",AssignOp,VSym \"x\",BOp ExpOp,VSym \"x\",Semi])"
  , "(sr [] [VSym \"x\",AssignOp,VSym \"x\",BOp ModOp,VSym \"x\",Semi])"
  , "(sr [] [VSym \"x\",AssignOp,VSym \"x\",BOp DivOp,VSym \"x\",Semi])"
  , "(sr [] [VSym \"v\",AssignOp,CSym 5,BOp AddOp,VSym \"x\",Semi])"
  , "(sr [] [Keyword IfK,LPar,VSym \"x\",BOp SubOp,CSym 2,BOp GtOp,CSym 4,RPar,Keyword ThenK,Keyword ReturnK,CSym 4,Semi,Keyword ElseK,Keyword ReturnK,VSym \"x\",Semi])"
  , "(sr [] [Keyword IfK,BSym True,Keyword ThenK,Keyword ReturnK,CSym 5,Semi,Keyword ElseK,Keyword ReturnK,CSym 4,Semi])"
  , "(sr [] [Keyword IfK,NotOp,LPar,BSym False,BOp OrOp,LPar,BSym True,BOp AndOp,BSym True,RPar,RPar,Keyword ThenK,Keyword ReturnK,CSym 2,Semi])"
  , "(sr [] [Keyword IfK,LPar,VSym \"x\",BOp EqOp,CSym 5,BOp OrOp,VSym \"x\",BOp LteOp,CSym 5,BOp OrOp,VSym \"x\",BOp GteOp,CSym 5,BOp OrOp,VSym \"x\",BOp LtOp,CSym 5,BOp OrOp,VSym \"x\",BOp GtOp,CSym 5,RPar,Keyword ThenK,Keyword ReturnK,CSym 6,Semi])"
  , "(sr [] [Keyword WhileK,LPar,VSym \"x\",BOp NeqOp,CSym 5,RPar,LBra,Keyword ReturnK,CSym 6,Semi,Keyword ReturnK,CSym 7,Semi,RBra])"
  , "(evala [(\"x\", 5), (\"y\", 4)] (Add (Var \"x\") (Div (Var \"y\") (Num 2))))"
  , "(evala [(\"x\", 5)] (Div (Mod (Div (Var \"x\") (Num 3)) (Num 6)) (Num 3)))"
  , "(evala [(\"x\", 4)] (Sub (Add (Mul (Var \"x\") (Num 3)) (Num 3)) (Mod (Num 3) (Exp (Num 6) (Num 3)))))"
  , "(evala [] (Sub (Num 3) (Num 1)))"
  , "(evala [] (Div (Num 3) (Num 1)))"
  , "(evalb [(\"x\", 4)] (And (Const True) (Lt (Var \"x\") (Num 3))))"
  , "(evalb [(\"x\", 6)] (Or (Gte (Var \"x\") (Num 2)) (Lte (Var \"x\") (Num 3))))"
  , "(evalb [(\"x\", 2)] (Not (Neq (Var \"x\") (Num 2))))"
  , "(evalb [(\"x\", 2)] (Eq (Var \"x\") (Num 2)))"
  , "(evalb [(\"x\", 3)] (And (Gt (Var \"x\") (Num 2)) (Const False)))"
  , "(execList (reverse [(IfThen (Eq (Mod (Var \"n\") (Num 2)) (Num 0)) (Return (Num 0))), (Assign \"n\" (Num 91))]) [])"
  , "(execList (reverse [(While (Lt (Var \"i\") (Var \"n\")) (Do [IfThen (Eq (Mod (Var \"n\") (Var \"i\")) (Num 0)) (Return (Num 0)),Assign \"i\" (Add (Var \"i\") (Num 2))]))]) [(\"n\",91), (\"i\",3)])"
  , "(execList (reverse [(While (Lt (Var \"i\") (Var \"n\")) (Do [IfThen (Eq (Mod (Var \"n\") (Var \"i\")) (Num 0)) (Return (Num 0)),Assign \"i\" (Add (Var \"i\") (Num 2))]))]) [(\"n\",97), (\"i\",3)])"
  , "(execList (reverse [(Return (Var \"z\")), (Assign \"z\" (Add (Mul (Num 2) (Var \"x\")) (Mod (Var \"y\") (Exp (Num 3) (Num 2))))), (Assign \"y\" (Num 10)), (Assign \"x\" (Num 5))]) [])"
  , "(execList (reverse [(Return (Var \"sum\")), (While (Lt (Var \"c\") (Var \"max\")) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"sum\" (Add (Var \"sum\") (Var \"c\"))])), (Assign \"sum\" (Num 0)), (Assign \"max\" (Num 100)), (Assign \"c\" (Num 0))]) [])"
  , "(execList (reverse [(Return (Var \"x\")), (While (Lte (Var \"c\") (Sub (Var \"fib\") (Num 1))) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"z\" (Add (Var \"x\") (Var \"y\")),Assign \"x\" (Var \"y\"),Assign \"y\" (Var \"z\")])), (Assign \"c\" (Num 0)), (Assign \"y\" (Num 1)), (Assign \"x\" (Num 0)), (Assign \"fib\" (Num 8))]) [])"
  , "(execList (reverse [(Return (Var \"acc\")), (While (Not (Lte (Var \"fact\") (Var \"c\"))) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))])), (Assign \"c\" (Num 1)), (Assign \"acc\" (Num 1)), (Assign \"fact\" (Num 5))]) [])"
  , "(execList (reverse [(Return (Var \"sum\")), (While (Lt (Var \"i\") (Var \"x\")) (Do [Assign \"sum\" (Add (Var \"sum\") (Var \"y\")),Assign \"i\" (Add (Var \"i\") (Num 1))])), (Assign \"i\" (Num 0)), (Assign \"sum\" (Num 0)), (Assign \"y\" (Num 7)), (Assign \"x\" (Num 6))]) [])"
  , "(execList (reverse [(Return (Var \"acc\")), (While (Not (Lte (Var \"fact\") (Var \"c\"))) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))])), (Assign \"c\" (Num 1)), (Assign \"acc\" (Num 1))]) [(\"fact\", 8)])"
  , "(execList (reverse [(Return (Var \"x\")), (While (Lte (Var \"c\") (Sub (Var \"fib\") (Num 1))) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"z\" (Add (Var \"x\") (Var \"y\")),Assign \"x\" (Var \"y\"),Assign \"y\" (Var \"z\")])), (Assign \"c\" (Num 0)), (Assign \"y\" (Num 1)), (Assign \"x\" (Num 0))]) [(\"fib\", 9)])"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
