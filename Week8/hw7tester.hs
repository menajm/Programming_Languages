subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (flip elem ys) xs

eq :: (Eq a) => [a] -> [a] -> Bool
eq xs ys = subset xs ys && subset ys xs

------ Tautologies -------
prop1 = Or (Not (Var "X")) (Var "X")
prop2 = Const True
prop3 = Or (Const True) (Var "X")
prop8 = Iff prop4 prop5
prop9 = Iff prop6 prop7

----- Contradictions -----
prop4 = Const False
prop5 = And (Const False) (Var "X")

---------- Demorgan ---------
prop6 = Not (Or (Var "X") (Var "Y")) -- !(X \/ Y)
prop7 = And (Not (Var "X")) (Not (Var "Y")) -- !X /\ !Y

-------- Transitivity -------
prop10 = And (Imp (Var "X") (Var "Y")) (Imp (Var "Y") (Var "Z"))
prop11 = Imp (Var "X") (Var "Z")
prop12 = Iff prop10 prop11
prop13 = Imp (And prop11 (Var "X")) (Var "Z")

---------- Misc ------------
prop14 = Iff (Imp (And (Var "X") (Var "Y")) (Var "Z"))
             (Or (Imp (Or (Var "A") (Const True)) (Var "B")) (And (Var "C") (Var "D")))
prop15 = And (Not (Var "X")) (Var "X") -- Also a contradiction

tests =
  [ ((fv prop2) `eq` [])
  , ((fv prop5) `eq` ["X"])
  , ((fv prop6) `eq` ["X","Y"])
  , ((fv prop7) `eq` ["X","Y"])
  , ((fv prop10) `eq` ["X","Y","Z"])
  , ((fv prop11) `eq` ["X","Z"])
  , ((fv prop12) `eq` ["X","Y","Z"])
  , ((fv prop13) `eq` ["X","Z"])
  , ((fv prop14) `eq` ["X","Y","Z","A","B","C","D"])
  , ((fv prop15) `eq` ["X"])
  , ((eval [("X", True)] prop5) == False)
  , ((eval [("X", False), ("Z", True)] prop13) == True)
  , ((eval [("X", True), ("Z", True)] prop13) == True)
  , ((eval [("X", True), ("Y", True), ("Z", True)] prop12) == True)
  , ((eval [("X", False), ("Y", True), ("Z", False)] prop12) == False)
  , ((eval [("X", False), ("Y", True), ("Z", True)] prop12) == True)
  , ((eval [("X", False), ("Z", True)] prop11) == True)
  , ((eval [("X", True), ("Z", True)] prop11) == True)
  , ((eval [("X", False), ("Y", True)] prop9) == True)
  , ((eval [("X", True), ("Y", False)] prop9) == True)
  , ((contra prop4 ) == True)
  , ((contra prop5 ) == True)
  , ((contra prop6) == False)
  , ((contra prop7) == False)
  , ((contra (Imp (Not (prop6)) prop7)) == False)
  , ((contra prop12) == False)
  , ((contra prop13) == False)
  , ((contra (Not (prop1))) == True)
  , ((contra prop15) == True)
  , ((contra prop10) == False)
  , ((tauto prop4 ) == False)
  , ((tauto prop5 ) == False)
  , ((tauto prop6) == False)
  , ((tauto prop7) == False)
  , ((tauto (Imp (Not (prop6)) prop7)) == False)
  , ((tauto prop12) == False)
  , ((tauto prop13) == True)
  , ((tauto (Not (prop1))) == False)
  , ((tauto prop15) == False)
  , ((tauto prop10) == False)
  , ((findSat prop1) == Just [("X",False)])
  , ((findSat prop3) == Just [("X",False)])
  , ((findSat prop6) == Just [("X",False),("Y",False)])
  , ((findSat prop7) == Just [("X",False),("Y",False)])
  , ((findSat prop8) == Just [("X",False)])
  , ((findSat prop9) == Just [("X",False),("Y",False)])
  , ((findSat prop10) == Just [("X",False),("Y",False),("Z",False)])
  , ((findSat prop11) == Just [("X",False),("Z",False)])
  , ((findSat prop14) == Just [("X",False),("Y",False),("Z",False),("A",False),("B",False),("C",True),("D",True)])
  , ((findSat prop15) == Nothing)
  , ((findRefute prop1) == Nothing)
  , ((findRefute prop3) == Nothing)
  , ((findRefute prop6) == Just [("X",False),("Y",True)])
  , ((findRefute prop7) == Just [("X",False),("Y",True)])
  , ((findRefute prop8) == Nothing)
  , ((findRefute prop9) == Nothing)
  , ((findRefute prop10) == Just [("X",False),("Y",True),("Z",False)])
  , ((findRefute prop11) == Just [("X",True),("Z",False)])
  , ((findRefute prop14) == Just [("X",False),("Y",False),("Z",False),("A",False),("B",False),("C",False),("D",False)])
  , ((findRefute prop15) == Just [("X",False)])
  , ((classify prop1) == "tautology")
  , ((classify prop3) == "tautology")
  , ((classify prop6) == "contingency")
  , ((classify prop7) == "contingency")
  , ((classify prop8) == "tautology")
  , ((classify prop9) == "tautology")
  , ((classify prop10) == "contingency")
  , ((classify prop11) == "contingency")
  , ((classify prop14) == "contingency")
  , ((classify prop15) == "contradiction")
  , ((checkEq prop1 prop2) == True)
  , ((checkEq prop1 prop3) == True)
  , ((checkEq prop6 prop7) == True)
  , ((checkEq prop8 prop9) == True)
  , ((checkEq prop8 prop4) == False)
  , ((checkEq prop12 prop8) == False)
  , ((checkEq prop15 prop5) == True)
  , ((checkEq prop14 prop10) == False)
  , ((checkEq prop6 prop11) == False)
  , ((checkEq prop10 prop7) == False)
  , ((refuteEq prop1 prop2) == Nothing)
  , ((refuteEq prop1 prop3) == Nothing)
  , ((refuteEq prop6 prop7) == Nothing)
  , ((refuteEq prop8 prop9) == Nothing)
  , ((refuteEq prop8 prop4) == Just [("X",False)])
  , ((refuteEq prop12 prop8) == Just [("X",False),("Y",True),("Z",False)])
  , ((refuteEq prop15 prop5) == Nothing)
  , ((refuteEq prop14 prop10) == Just [("X",False),("Y",False),("Z",False),("A",False),("B",False),("C",False),("D",False)])
  , ((refuteEq prop6 prop11) == Just [("X",False),("Y",True),("Z",False)])
  , ((refuteEq prop10 prop7) == Just [("X",False),("Y",True),("Z",True)])
  , ((lexer "X") == [VSym "X"])
  , ((lexer "X /\\ Y") == [VSym "X",BOp AndOp,VSym "Y"])
  , ((lexer "X /\\ (Y /\\ Z)") == [VSym "X",BOp AndOp,LPar,VSym "Y",BOp AndOp,VSym "Z",RPar])
  , ((lexer "! (X \\/ Y)") == [NotOp,LPar,VSym "X",BOp OrOp,VSym "Y",RPar])
  , ((lexer "!!X -> X") == [NotOp,NotOp,VSym "X",BOp ImpOp,VSym "X"])
  , ((lexer "!X /\\ !Y") == [NotOp,VSym "X",BOp AndOp,NotOp,VSym "Y"])
  , ((lexer "tt") == [CSym True])
  , ((lexer "X \\/ ff") == [VSym "X",BOp OrOp,CSym False])
  , ((lexer "X <-> Y") == [VSym "X",BOp IffOp,VSym "Y"])
  , ((lexer "!X -> Y") == [NotOp,VSym "X",BOp ImpOp,VSym "Y"])
  , ((sr [] [VSym "X"] ) == [PB (Var "X")])
  , ((sr [] [VSym "X",BOp AndOp,VSym "Y"] ) == [PB (And (Var "X") (Var "Y"))])
  , ((sr [] [VSym "X",BOp AndOp,LPar,VSym "Y",BOp AndOp,VSym "Z",RPar] ) == [PB (And (Var "X") (And (Var "Y") (Var "Z")))])
  , ((sr [] [NotOp,LPar,VSym "X",BOp OrOp,VSym "Y",RPar] ) == [PB (Not (Or (Var "X") (Var "Y")))])
  , ((sr [] [NotOp,NotOp,VSym "X"] ) == [PB (Not (Not (Var "X")))])
  , ((sr [] [NotOp,VSym "X",BOp AndOp,NotOp,VSym "Y"] ) == [PB (And (Not (Var "X")) (Not (Var "Y")))])
  , ((sr [] [CSym True] ) == [PB (Const True)])
  , ((sr [] [VSym "X",BOp OrOp,CSym False] ) == [PB (Or (Var "X") (Const False))])
  , ((sr [] [VSym "X",BOp IffOp,VSym "Y"] ) == [PB (Iff (Var "X") (Var "Y"))])
  , ((sr [] [NotOp,VSym "X",BOp ImpOp,VSym "Y"] ) == [PB (Imp (Not (Var "X")) (Var "Y"))])
  , ((parseProp "X /\\ (ff /\\ Y)") == Left (And (Var "X") (And (Const False) (Var "Y"))))
  , ((parseProp "X \\/ Y") == Left (Or (Var "X") (Var "Y")))
  , ((parseProp "X /\\ (Y /\\ !Z)") == Left (And (Var "X") (And (Var "Y") (Not (Var "Z")))))
  , ((parseProp "! (X \\/ Y)") == Left (Not (Or (Var "X") (Var "Y"))))
  , ((parseProp "!!X") == Left (Not (Not (Var "X"))))
  , ((parseProp "!X /\\ !Y") == Left (And (Not (Var "X")) (Not (Var "Y"))))
  , ((parseProp "!(P /\\ Q) \\/ Q") == Left (Or (Not (And (Var "P") (Var "Q"))) (Var "Q")))
  , ((parseProp "Z <-> (X \\/ Y)") == Left (Iff (Var "Z") (Or (Var "X") (Var "Y"))))
  , ((parseProp "!(X /\\ Y) \\/ !(Y /\\ Z)") == Left (Or (Not (And (Var "X") (Var "Y"))) (Not (And (Var "Y") (Var "Z")))))
  , ((parseProp "(R <-> (S /\\ R)) <-> (! (Q /\\ P))") == Left (Iff (Iff (Var "R") (And (Var "S") (Var "R"))) (Not (And (Var "Q") (Var "P")))))
  ]
testLinesString =
  [ "(fv prop2)"
  , "(fv prop5)"
  , "(fv prop6)"
  , "(fv prop7)"
  , "(fv prop10)"
  , "(fv prop11)"
  , "(fv prop12)"
  , "(fv prop13)"
  , "(fv prop14)"
  , "(fv prop15)"
  , "(eval [(\"X\", True)] prop5)"
  , "(eval [(\"X\", False), (\"Z\", True)] prop13)"
  , "(eval [(\"X\", True), (\"Z\", True)] prop13)"
  , "(eval [(\"X\", True), (\"Y\", True), (\"Z\", True)] prop12)"
  , "(eval [(\"X\", False), (\"Y\", True), (\"Z\", False)] prop12)"
  , "(eval [(\"X\", False), (\"Y\", True), (\"Z\", True)] prop12)"
  , "(eval [(\"X\", False), (\"Z\", True)] prop11)"
  , "(eval [(\"X\", True), (\"Z\", True)] prop11)"
  , "(eval [(\"X\", False), (\"Y\", True)] prop9)"
  , "(eval [(\"X\", True), (\"Y\", False)] prop9)"
  , "(contra prop4 )"
  , "(contra prop5 )"
  , "(contra prop6)"
  , "(contra prop7)"
  , "(contra (Imp (Not (prop6)) prop7))"
  , "(contra prop12)"
  , "(contra prop13)"
  , "(contra (Not (prop1)))"
  , "(contra prop15)"
  , "(contra prop10)"
  , "(tauto prop4 )"
  , "(tauto prop5 )"
  , "(tauto prop6)"
  , "(tauto prop7)"
  , "(tauto (Imp (Not (prop6)) prop7))"
  , "(tauto prop12)"
  , "(tauto prop13)"
  , "(tauto (Not (prop1)))"
  , "(tauto prop15)"
  , "(tauto prop10)"
  , "(findSat prop1)"
  , "(findSat prop3)"
  , "(findSat prop6)"
  , "(findSat prop7)"
  , "(findSat prop8)"
  , "(findSat prop9)"
  , "(findSat prop10)"
  , "(findSat prop11)"
  , "(findSat prop14)"
  , "(findSat prop15)"
  , "(findRefute prop1)"
  , "(findRefute prop3)"
  , "(findRefute prop6)"
  , "(findRefute prop7)"
  , "(findRefute prop8)"
  , "(findRefute prop9)"
  , "(findRefute prop10)"
  , "(findRefute prop11)"
  , "(findRefute prop14)"
  , "(findRefute prop15)"
  , "(classify prop1)"
  , "(classify prop3)"
  , "(classify prop6)"
  , "(classify prop7)"
  , "(classify prop8)"
  , "(classify prop9)"
  , "(classify prop10)"
  , "(classify prop11)"
  , "(classify prop14)"
  , "(classify prop15)"
  , "(checkEq prop1 prop2)"
  , "(checkEq prop1 prop3)"
  , "(checkEq prop6 prop7)"
  , "(checkEq prop8 prop9)"
  , "(checkEq prop8 prop4)"
  , "(checkEq prop12 prop8)"
  , "(checkEq prop15 prop5)"
  , "(checkEq prop14 prop10)"
  , "(checkEq prop6 prop11)"
  , "(checkEq prop10 prop7)"
  , "(refuteEq prop1 prop2)"
  , "(refuteEq prop1 prop3)"
  , "(refuteEq prop6 prop7)"
  , "(refuteEq prop8 prop9)"
  , "(refuteEq prop8 prop4)"
  , "(refuteEq prop12 prop8)"
  , "(refuteEq prop15 prop5)"
  , "(refuteEq prop14 prop10)"
  , "(refuteEq prop6 prop11)"
  , "(refuteEq prop10 prop7)"
  , "(lexer \"X\")"
  , "(lexer \"X /\\\\ Y\")"
  , "(lexer \"X /\\\\ (Y /\\\\ Z)\")"
  , "(lexer \"! (X \\\\/ Y)\")"
  , "(lexer \"!!X -> X\")"
  , "(lexer \"!X /\\\\ !Y\")"
  , "(lexer \"tt\")"
  , "(lexer \"X \\\\/ ff\")"
  , "(lexer \"X <-> Y\")"
  , "(lexer \"!X -> Y\")"
  , "(sr [] [VSym \"X\"] )"
  , "(sr [] [VSym \"X\",BOp AndOp,VSym \"Y\"] )"
  , "(sr [] [VSym \"X\",BOp AndOp,LPar,VSym \"Y\",BOp AndOp,VSym \"Z\",RPar] )"
  , "(sr [] [NotOp,LPar,VSym \"X\",BOp OrOp,VSym \"Y\",RPar] )"
  , "(sr [] [NotOp,NotOp,VSym \"X\"] )"
  , "(sr [] [NotOp,VSym \"X\",BOp AndOp,NotOp,VSym \"Y\"] )"
  , "(sr [] [CSym True] )"
  , "(sr [] [VSym \"X\",BOp OrOp,CSym False] )"
  , "(sr [] [VSym \"X\",BOp IffOp,VSym \"Y\"] )"
  , "(sr [] [NotOp,VSym \"X\",BOp ImpOp,VSym \"Y\"] )"
  , "(parseProp \"X /\\\\ (ff /\\\\ Y)\")"
  , "(parseProp \"X \\\\/ Y\")"
  , "(parseProp \"X /\\\\ (Y /\\\\ !Z)\")"
  , "(parseProp \"! (X \\\\/ Y)\")"
  , "(parseProp \"!!X\")"
  , "(parseProp \"!X /\\\\ !Y\")"
  , "(parseProp \"!(P /\\\\ Q) \\\\/ Q\")"
  , "(parseProp \"Z <-> (X \\\\/ Y)\")"
  , "(parseProp \"!(X /\\\\ Y) \\\\/ !(Y /\\\\ Z)\")"
  , "(parseProp \"(R <-> (S /\\\\ R)) <-> (! (Q /\\\\ P))\")"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
