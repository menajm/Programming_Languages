
lTree1 :: LTree Integer
lTree1 = LLeaf 0

mTree1 :: MTree Integer
mTree1 = MLeaf 0

lTree2 :: LTree String
lTree2 = LNode "Daida" (LLeaf " afnif") (LLeaf "ADwn")

mTree2 :: MTree String
mTree2 = UNode "Plumbob" (BNode (MLeaf "andfdna") (MLeaf "bounce"))

lTree3 :: LTree (String, Integer)
lTree3 = LNode ("Fish", 6) (LLeaf ("Fish", 6)) (LLeaf ("hello", 3))

mTree3 :: MTree (String, Integer)
mTree3 = BNode (MLeaf ("Fish", 3)) (MLeaf ("Frog", 3))

lTree4 :: LTree Integer
lTree4 = LNode 1
               (LNode 2
                      (LNode 4 (LLeaf 4)
                               (LLeaf 5))
                      (LNode 1
                             (LNode 6
                                    (LNode 8
                                           (LLeaf 4)
                                           (LLeaf 4))
                                    (LLeaf 8))
                             (LLeaf 6)))
               (LLeaf 2)

mTree4 :: MTree Integer
mTree4 = UNode 1
               (BNode (UNode 4
                             (MLeaf 3))
                      (UNode 3
                             (BNode (MLeaf 1)
                                    (BNode (UNode 2
                                                  (MLeaf 2))
                                           (UNode 3
                                                  (MLeaf 4))))))

lTree5 :: LTree String
lTree5 = LNode "AbbabbA"
                (LLeaf "Dog")
                (LNode "Dog"
                       (LLeaf "Dog")
                       (LLeaf "Dog"))

mTree5 :: MTree String
mTree5 = UNode "drain-o"
               (UNode "Abbba"
                    (BNode (MLeaf "aad")
                            (MLeaf "drain-o")))

lTree6 :: LTree String
lTree6 = LNode "AbbabbA"
                (LLeaf "Dog")
                (LNode "Dog"
                       (LLeaf "Dog")
                       (LLeaf "Dog"))

mTree6 :: MTree String
mTree6 = UNode "drain-o"
               (BNode (UNode "drain-o"
                             (MLeaf "aad"))
                      (MLeaf "drain-o"))

lTree7 :: LTree Integer
lTree7 = LNode 4
              (LNode 6
                     (LLeaf 4)
                     (LLeaf 6))
              (LLeaf 6)

mTree7 :: MTree Integer
mTree7 = BNode (UNode 4
                      (UNode 6
                             (BNode (MLeaf 4)
                                    (MLeaf 6))))
               (MLeaf 6)

lTree8 :: LTree Integer
lTree8 = LNode 6
               (LNode 4
                      (LLeaf 4)
                      (LLeaf 6))
               (LLeaf 9)

mTree8 :: MTree Integer
mTree8 = BNode (UNode 4
                      (UNode 6
                             (BNode (MLeaf 4)
                                    (MLeaf 6))))
               (BNode (MLeaf 6)
                      (MLeaf 9))

lTree9 :: LTree Integer
lTree9 = LNode 9
               (LLeaf 8)
               (LNode 12
                      (LLeaf 10)
                      (LNode 12
                             (LNode 1
                                    (LLeaf 10)
                                    (LLeaf 10))
                             (LNode 12
                                    (LLeaf 12)
                                    (LLeaf 12))))

mTree9 :: MTree Integer
mTree9 = UNode 9
               (BNode (MLeaf 10)
                      (BNode (UNode 7
                                    (MLeaf 10))
                             (UNode 9
                                    (BNode (UNode 9
                                                  (BNode (MLeaf 10)
                                                         (UNode 9
                                                                (BNode (MLeaf 9)
                                                                       (MLeaf 12)))))
                                           (MLeaf 10)))))

lTree10 :: LTree String
lTree10 = LNode "DWOn"
                (LNode " adnoiw"
                       (LLeaf "DAW")
                       (LNode "seriptitiousuoititpires"
                              (LLeaf "Drain-o")
                              (LLeaf "seriptitiousuoititpires")))
                (LLeaf "DWOn")

mTree10 :: MTree String
mTree10 = UNode "badinu"
                (BNode (MLeaf "skqdko")
                       (UNode " seriptitiousuoititpires"
                              (UNode "Fish" (UNode "Daonwi"
                                            (BNode (MLeaf "seriptitiousuoititpires")
                                               (MLeaf "DAW"))))))
lTree11 :: LTree String
lTree11 = LLeaf "seriptitiousuoititpires"

mTree11 :: MTree String
mTree11 = MLeaf "seriptitiousuoititpires"


tests =
  [ ((getLLeaves lTree1) == [0])
  , ((getLLeaves lTree2) == [" afnif","ADwn"])
  , ((getLLeaves lTree3) == [("Fish",6),("hello",3)])
  , ((getLLeaves lTree4) == [4,5,4,4,8,6,2])
  , ((getLLeaves lTree5) == ["Dog","Dog","Dog"])
  , ((getMLeaves mTree1) == [0])
  , ((getMLeaves mTree2) == ["andfdna","bounce"])
  , ((getMLeaves mTree3) == [("Fish",3),("Frog",3)])
  , ((getMLeaves mTree4) == [3,1,2,4])
  , ((getMLeaves mTree5) == ["aad","drain-o"])
  , ((maxLDepth lTree1) == 0)
  , ((maxLDepth lTree4) == 5)
  , ((maxLDepth lTree7) == 2)
  , ((maxLDepth lTree8) == 2)
  , ((maxLDepth lTree9) == 4)
  , ((maxMDepth mTree1) == 0)
  , ((maxMDepth mTree4) == 6)
  , ((maxMDepth mTree7) == 4)
  , ((maxMDepth mTree8) == 4)
  , ((maxMDepth mTree9) == 9)
  , ((maxLTree lTree1) == 0)
  , ((maxLTree lTree4) == 8)
  , ((maxLTree lTree7) == 6)
  , ((maxLTree lTree8) == 9)
  , ((maxLTree lTree9) == 12)
  , ((maxMDepth mTree1) == 0)
  , ((maxMDepth mTree4) == 6)
  , ((maxMDepth mTree7) == 4)
  , ((maxMDepth mTree8) == 4)
  , ((maxMDepth mTree9) == 9)
  , ((uncoveredLeafL 4 lTree4) == True)
  , ((uncoveredLeafL 2 lTree4) == True)
  , ((uncoveredLeafL 4 lTree7) == False)
  , ((uncoveredLeafL 9 lTree8) == True)
  , ((uncoveredLeafL 34 lTree8) == False)
  , ((uncoveredLeafM 4 mTree4) == True)
  , ((uncoveredLeafM 3 mTree4) == True)
  , ((uncoveredLeafM 6 mTree7) == True)
  , ((uncoveredLeafM 9 mTree8) == True)
  , ((uncoveredLeafM 34 mTree8) == False)
  , ((mapLTree (\x -> x * x) lTree9) == LNode 81 (LLeaf 64) (LNode 144 (LLeaf 100) (LNode 144 (LNode 1 (LLeaf 100) (LLeaf 100)) (LNode 144 (LLeaf 144) (LLeaf 144)))))
  , ((mapLTree (\x -> x * x) lTree4) == LNode 1 (LNode 4 (LNode 16 (LLeaf 16) (LLeaf 25)) (LNode 1 (LNode 36 (LNode 64 (LLeaf 16) (LLeaf 16)) (LLeaf 64)) (LLeaf 36))) (LLeaf 4))
  , ((mapLTree (\x -> reverse x == x) lTree5) == LNode True (LLeaf False) (LNode False (LLeaf False) (LLeaf False)))
  , ((mapLTree (\x -> (fst x == "jello")) lTree3) == LNode False (LLeaf False) (LLeaf False))
  , ((mapLTree (\x -> x > 5) lTree7 ) == LNode False (LNode True (LLeaf False) (LLeaf True)) (LLeaf True))
  , ((mapMTree (\x -> x * x) mTree9) == UNode 81 (BNode (MLeaf 100) (BNode (UNode 49 (MLeaf 100)) (UNode 81 (BNode (UNode 81 (BNode (MLeaf 100) (UNode 81 (BNode (MLeaf 81) (MLeaf 144))))) (MLeaf 100))))))
  , ((mapMTree (\x -> x * x) mTree4) == UNode 1 (BNode (UNode 16 (MLeaf 9)) (UNode 9 (BNode (MLeaf 1) (BNode (UNode 4 (MLeaf 4)) (UNode 9 (MLeaf 16)))))))
  , ((mapMTree (\x -> reverse x == x) mTree5) == UNode False (UNode False (BNode (MLeaf False) (MLeaf False))))
  , ((mapMTree (\x -> (fst x == "jello")) mTree3) == BNode (MLeaf False) (MLeaf False))
  , ((mapMTree (\x -> x > 5) mTree7) == BNode (UNode False (UNode True (BNode (MLeaf False) (MLeaf True)))) (MLeaf True))
  , ((applyLfun lTree1) == LLeaf 1)
  , ((applyLfun lTree4) == LNode 1 (LNode 14 (LNode 65532 (LLeaf 65532) (LLeaf 33554427)) (LNode 1 (LNode 68719476730 (LNode 18446744073709551608 (LLeaf 65532) (LLeaf 65532)) (LLeaf 18446744073709551608)) (LLeaf 68719476730))) (LLeaf 14))
  , ((applyLfun lTree7) == LNode 65532 (LNode 68719476730 (LLeaf 65532) (LLeaf 68719476730)) (LLeaf 68719476730))
  , ((applyLfun lTree8) == LNode 68719476730 (LNode 65532 (LLeaf 65532) (LLeaf 68719476730)) (LLeaf 2417851639229258349412343))
  , ((applyLfun lTree9) == LNode 2417851639229258349412343 (LLeaf 18446744073709551608) (LNode 22300745198530623141535718272648361505980404 (LLeaf 1267650600228229401496703205366) (LNode 22300745198530623141535718272648361505980404 (LNode 1 (LLeaf 1267650600228229401496703205366) (LLeaf 1267650600228229401496703205366)) (LNode 22300745198530623141535718272648361505980404 (LLeaf 22300745198530623141535718272648361505980404) (LLeaf 22300745198530623141535718272648361505980404)))))
  , ((applyMfun mTree1) == MLeaf 1)
  , ((applyMfun mTree4) == UNode 1 (BNode (UNode 65532 (MLeaf 509)) (UNode 509 (BNode (MLeaf 1) (BNode (UNode 14 (MLeaf 14)) (UNode 509 (MLeaf 65532)))))))
  , ((applyMfun mTree7) == BNode (UNode 65532 (UNode 68719476730 (BNode (MLeaf 65532) (MLeaf 68719476730)))) (MLeaf 68719476730))
  , ((applyMfun mTree8) == BNode (UNode 65532 (UNode 68719476730 (BNode (MLeaf 65532) (MLeaf 68719476730)))) (BNode (MLeaf 68719476730) (MLeaf 2417851639229258349412343)))
  , ((applyMfun mTree9) == UNode 2417851639229258349412343 (BNode (MLeaf 1267650600228229401496703205366) (BNode (UNode 562949953421305 (MLeaf 1267650600228229401496703205366)) (UNode 2417851639229258349412343 (BNode (UNode 2417851639229258349412343 (BNode (MLeaf 1267650600228229401496703205366) (UNode 2417851639229258349412343 (BNode (MLeaf 2417851639229258349412343) (MLeaf 22300745198530623141535718272648361505980404))))) (MLeaf 1267650600228229401496703205366))))))
  , ((findLTree (\x -> reverse x == x) lTree5) == Just "AbbabbA")
  , ((findLTree (\x -> (fst x == "jello")) lTree3) == Nothing)
  , ((findLTree (\x -> x > 5) lTree7) == Just 6)
  , ((findLTree (\x -> x == 12) lTree9) == Just 12)
  , ((findLTree (\x -> x == 3) lTree4) == Nothing)
  , ((findMTree (\x -> reverse x == x) mTree5) == Nothing)
  , ((findMTree (\x -> (fst x == "jello")) mTree3) == Nothing)
  , ((findMTree (\x -> x > 5) mTree7) == Just 6)
  , ((findMTree (\x -> x == 12) mTree9) == Just 12)
  , ((findMTree (\x -> x == 3) mTree4) == Just 3)
  , ((findLpali lTree2) == Nothing)
  , ((findLpali lTree5) == Just "AbbabbA")
  , ((findLpali lTree6) == Just "AbbabbA")
  , ((findLpali lTree10) == Just "seriptitiousuoititpires")
  , ((findLpali lTree11) == Just "seriptitiousuoititpires")
  , ((findMpali mTree2) == Just "andfdna")
  , ((findMpali mTree5) == Nothing)
  , ((findMpali mTree6) == Nothing)
  , ((findMpali mTree10) == Just "seriptitiousuoititpires")
  , ((findMpali mTree11) == Just "seriptitiousuoititpires")
  , ((getLLeaves' lTree1) == [0])
  , ((getLLeaves' lTree2) == [" afnif","ADwn"])
  , ((getLLeaves' lTree3) == [("Fish",6),("hello",3)])
  , ((getLLeaves' lTree4) == [4,5,4,4,8,6,2])
  , ((getLLeaves' lTree5) == ["Dog","Dog","Dog"])
  , ((getMLeaves' mTree1) == [0])
  , ((getMLeaves' mTree2) == ["andfdna","bounce"])
  , ((getMLeaves' mTree3) == [("Fish",3),("Frog",3)])
  , ((getMLeaves' mTree4) == [3,1,2,4])
  , ((getMLeaves' mTree5) == ["aad","drain-o"])
  , ((uncoveredLeafL' 4 lTree7) == False)
  , ((uncoveredLeafL' 6 lTree7) == True)
  , ((uncoveredLeafL' 6 lTree8) == False)
  , ((uncoveredLeafL' 9 lTree8) == True)
  , ((uncoveredLeafL' 34 lTree8) == False)
  , ((uncoveredLeafM' 4 mTree7) == False)
  , ((uncoveredLeafM' 6 mTree7) == True)
  , ((uncoveredLeafM' 6 mTree8) == True)
  , ((uncoveredLeafM' 9 mTree8) == True)
  , ((uncoveredLeafM' 34 mTree8) == False)
  ]
testLinesString =
  [ "(getLLeaves lTree1)"
  , "(getLLeaves lTree2)"
  , "(getLLeaves lTree3)"
  , "(getLLeaves lTree4)"
  , "(getLLeaves lTree5)"
  , "(getMLeaves mTree1)"
  , "(getMLeaves mTree2)"
  , "(getMLeaves mTree3)"
  , "(getMLeaves mTree4)"
  , "(getMLeaves mTree5)"
  , "(maxLDepth lTree1)"
  , "(maxLDepth lTree4)"
  , "(maxLDepth lTree7)"
  , "(maxLDepth lTree8)"
  , "(maxLDepth lTree9)"
  , "(maxMDepth mTree1)"
  , "(maxMDepth mTree4)"
  , "(maxMDepth mTree7)"
  , "(maxMDepth mTree8)"
  , "(maxMDepth mTree9)"
  , "(maxLTree lTree1)"
  , "(maxLTree lTree4)"
  , "(maxLTree lTree7)"
  , "(maxLTree lTree8)"
  , "(maxLTree lTree9)"
  , "(maxMDepth mTree1)"
  , "(maxMDepth mTree4)"
  , "(maxMDepth mTree7)"
  , "(maxMDepth mTree8)"
  , "(maxMDepth mTree9)"
  , "(uncoveredLeafL 4 lTree4)"
  , "(uncoveredLeafL 2 lTree4)"
  , "(uncoveredLeafL 4 lTree7)"
  , "(uncoveredLeafL 9 lTree8)"
  , "(uncoveredLeafL 34 lTree8)"
  , "(uncoveredLeafM 4 mTree4)"
  , "(uncoveredLeafM 3 mTree4)"
  , "(uncoveredLeafM 6 mTree7)"
  , "(uncoveredLeafM 9 mTree8)"
  , "(uncoveredLeafM 34 mTree8)"
  , "(mapLTree (\\x -> x * x) lTree9)"
  , "(mapLTree (\\x -> x * x) lTree4)"
  , "(mapLTree (\\x -> reverse x == x) lTree5)"
  , "(mapLTree (\\x -> (fst x == \"jello\")) lTree3)"
  , "(mapLTree (\\x -> x > 5) lTree7 )"
  , "(mapMTree (\\x -> x * x) mTree9)"
  , "(mapMTree (\\x -> x * x) mTree4)"
  , "(mapMTree (\\x -> reverse x == x) mTree5)"
  , "(mapMTree (\\x -> (fst x == \"jello\")) mTree3)"
  , "(mapMTree (\\x -> x > 5) mTree7)"
  , "(applyLfun lTree1)"
  , "(applyLfun lTree4)"
  , "(applyLfun lTree7)"
  , "(applyLfun lTree8)"
  , "(applyLfun lTree9)"
  , "(applyMfun mTree1)"
  , "(applyMfun mTree4)"
  , "(applyMfun mTree7)"
  , "(applyMfun mTree8)"
  , "(applyMfun mTree9)"
  , "(findLTree (\\x -> reverse x == x) lTree5)"
  , "(findLTree (\\x -> (fst x == \"jello\")) lTree3)"
  , "(findLTree (\\x -> x > 5) lTree7)"
  , "(findLTree (\\x -> x == 12) lTree9)"
  , "(findLTree (\\x -> x == 3) lTree4)"
  , "(findMTree (\\x -> reverse x == x) mTree5)"
  , "(findMTree (\\x -> (fst x == \"jello\")) mTree3)"
  , "(findMTree (\\x -> x > 5) mTree7)"
  , "(findMTree (\\x -> x == 12) mTree9)"
  , "(findMTree (\\x -> x == 3) mTree4)"
  , "(findLpali lTree2)"
  , "(findLpali lTree5)"
  , "(findLpali lTree6)"
  , "(findLpali lTree10)"
  , "(findLpali lTree11)"
  , "(findMpali mTree2)"
  , "(findMpali mTree5)"
  , "(findMpali mTree6)"
  , "(findMpali mTree10)"
  , "(findMpali mTree11)"
  , "(getLLeaves' lTree1)"
  , "(getLLeaves' lTree2)"
  , "(getLLeaves' lTree3)"
  , "(getLLeaves' lTree4)"
  , "(getLLeaves' lTree5)"
  , "(getMLeaves' mTree1)"
  , "(getMLeaves' mTree2)"
  , "(getMLeaves' mTree3)"
  , "(getMLeaves' mTree4)"
  , "(getMLeaves' mTree5)"
  , "(uncoveredLeafL' 4 lTree7)"
  , "(uncoveredLeafL' 6 lTree7)"
  , "(uncoveredLeafL' 6 lTree8)"
  , "(uncoveredLeafL' 9 lTree8)"
  , "(uncoveredLeafL' 34 lTree8)"
  , "(uncoveredLeafM' 4 mTree7)"
  , "(uncoveredLeafM' 6 mTree7)"
  , "(uncoveredLeafM' 6 mTree8)"
  , "(uncoveredLeafM' 9 mTree8)"
  , "(uncoveredLeafM' 34 mTree8)"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
