
tests =
  [ ((radius 18.021375982802088 173.33510724369103) == 174.26941612197874)
  , ((radius 0 412.32913662440524) == 412.32913662440524)
  , ((radius 0 0) == 0.0)
  , ((radius 67.93296283710589 0) == 67.93296283710589)
  , ((radius 0.5440756915691 0.2303903996966) == 0.5908452372904037)
  , ((radius (-311.34567118802806) (-240.61110395188845)) == 393.4842186320439)
  , ((radius (-212.73760404278133) 208.10481117806063) == 297.59855612774646)
  , ((radius 470.09681510124034 (-269.8274333824833)) == 542.0312346849656)
  , ((radius 0.3987925468672694 3.547137081723859) == 3.569484132473114)
  , ((radius 150.58107146415188 214.24393399315437) == 261.86852108673753)
  , ((sumEvens 0) == 0)
  , ((sumEvens 1) == 0)
  , ((sumEvens 5) == 6)
  , ((sumEvens 7) == 12)
  , ((sumEvens 10) == 30)
  , ((sumEvens 1000) == 250500)
  , ((sumEvens 12010) == 36066030)
  , ((sumEvens 21391) == 114393720)
  , ((sumEvens 11987) == 35922042)
  , ((sumEvens 20011) == 100110030)
  , ((multiplyEnds []) == 1)
  , ((multiplyEnds [0]) == 0)
  , ((multiplyEnds [10]) == 100)
  , ((multiplyEnds [1, 2, 3]) == 3)
  , ((multiplyEnds [40593, 27417]) == 1112938281)
  , ((multiplyEnds [35149, 87651, 35911, 83397, 99370]) == 3492756130)
  , ((multiplyEnds [38420, 40661, 13332, 8795]) == 337903900)
  , ((multiplyEnds [43593, 14552, 3952, 90453, 40925, 85543]) == 3729075999)
  , ((multiplyEnds [299, 68816, 9249, 13903, 71450, 92730, 98639, 86378]) == 25827022)
  , ((multiplyEnds [98030, 43375, 13656, 31539, 60734, 30991, 1092]) == 107048760)
  , ((getLengths []) == [])
  , ((getLengths ["hello", "world"]) == [5,5])
  , ((getLengths ["", "", "", "a"]) == [0,0,0,1])
  , ((getLengths ["lorem", "ipsum", "dolor", "sit", "amet"]) == [5,5,5,3,4])
  , ((getLengths ["1", "12", "123", "1234", "12345", "123456", "1234567"]) == [1,2,3,4,5,6,7])
  , ((getLengths ["[]", "[][][]", "[][", "[[[]]]"]) == [2,6,3,6])
  , ((getLengths ["[qn5MY6(dH", "dmGiRCExcA8CODW6pKIXU5", "z3RVBj"]) == [10,22,6])
  , ((getLengths ["6RXim2_mGXbb]VEWU", "6LS]U_3JZuaaCRM2C", "z", "ySNI(", "oc[6sBBvoeIC7CXTc[kKuY", "", "tARQnW", "B", "uDB_6CsE4zk9dLN", "CZNRMZlTdiktBltJG125"]) == [17,17,1,5,22,0,6,1,15,20])
  , ((getLengths ["((H5groMY[V-", "B3K", "3L]ECyVEgyBZ", "_H[6lLX-", "Y3W3]h-l", "Y4bWF_]7wZmJh", "DDvnAOc", "-bV9AS7y5bn"]) == [12,3,12,8,8,13,7,11])
  , ((getLengths ["wFJeZ7bk9C2KQuM9", "9zM4PI[", "H8sbR_FQqm(", "oCiBL", "a]WpuXJExZ", "i3l-q6wOsq9RGG]UbAr", "gKUK4CLNLycHOjt", "yzW]EAGeVIx6W-", "(1Jsqgc9"]) == [16,7,11,5,10,19,15,14,8])
  , ((dropLastTwo [23505, 57977]) == [])
  , ((dropLastTwo [95098, 23505]) == [])
  , ((dropLastTwo [55239, 23808, 23505]) == [55239])
  , ((dropLastTwo [13830, 45840, 96462]) == [13830])
  , ((dropLastTwo [15558, 92120, 51217, 26348]) == [15558,92120])
  , ((dropLastTwo [27919, 16857, 22233, 51243, 15288]) == [27919,16857,22233])
  , ((dropLastTwo [3201, 87168, 21085, 45539, 78773, 46677, 98576, 65939, 82261]) == [3201,87168,21085,45539,78773,46677,98576])
  , ((dropLastTwo [73741, 60411, 95098, 67195, 59542, 42884, 23505, 47216]) == [73741,60411,95098,67195,59542,42884])
  , ((dropLastTwo [578, 42300, 88438, 74099, 36219, 7526, 46539, 45197, 14057]) == [578,42300,88438,74099,36219,7526,46539])
  , ((dropLastTwo [63710, 96256, 57977, 91623, 66038, 25309, 87389, 22091, 82434]) == [63710,96256,57977,91623,66038,25309,87389])
  , ((findEmpty ["X", "OwsyeGuTB", "", "EG"]) == True)
  , ((findEmpty ["eNWEWRSXN"]) == False)
  , ((findEmpty ["Baamslu", "cpajmk", "pTIizL"]) == False)
  , ((findEmpty ["AkUyepwvq", "lglAykB", "", "QX", "", "sFGplIKrq", "Rcb"]) == True)
  , ((findEmpty ["IUoHkfB", "kO", "IYzO", "LvSOgVtjx"]) == False)
  , ((findEmpty [""]) == True)
  , ((findEmpty ["iNukiw"]) == False)
  , ((findEmpty ["", "k", "VDXuT", "dKsaKnJYc"]) == True)
  , ((findEmpty ["rdSvhNCj", "gyeomNbt", "Tg", "LQ", "xO", "HNf", ""]) == True)
  , ((findEmpty ["s", "R", "Cr", "HcGTPxv", "KCvNH", "K", "VATn"]) == False)
  , ((checkPalindrome "aaabbbaaa") == True)
  , ((checkPalindrome "aaabbaaa") == True)
  , ((checkPalindrome "ababa") == True)
  , ((checkPalindrome "a") == True)
  , ((checkPalindrome "aaaaaaaaa") == True)
  , ((checkPalindrome "DIxeLSPiikG") == False)
  , ((checkPalindrome "tVt") == True)
  , ((checkPalindrome "nfhxHktB") == False)
  , ((checkPalindrome "IeIailw") == False)
  , ((checkPalindrome "ycTsOrH") == False)
  , ((checkSize [16, 12, 13]) == True)
  , ((checkSize [9, 9]) == False)
  , ((checkSize [6, 19, 7, 10, 15, 1, 2, 9, 6]) == False)
  , ((checkSize [15, 7, 10, 4, 3, 15, 19, 19, 5]) == True)
  , ((checkSize [15, 7, 16]) == True)
  , ((checkSize [20, 2]) == False)
  , ((checkSize [9]) == False)
  , ((checkSize []) == False)
  , ((checkSize [6, 2, 19, 3, 5, 3, 7]) == False)
  , ((checkSize [17, 16, 14, 11, 14, 17, 15]) == True)
  , ((checkAnySize 2 [19, 16, 7, 1, 18, 5, 6, 17, 12, 14, 7]) == True)
  , ((checkAnySize 0 [0]) == True)
  , ((checkAnySize 12 [12, 15, 10, 10, 10, 6, 14, 9, 9, 13, 5, 8, 5, 11]) == True)
  , ((checkAnySize 18 [11, 4, 1, 7, 14, 14, 7, 14, 7, 16, 6, 17, 15, 15, 18, 15, 18, 11]) == False)
  , ((checkAnySize 9 [6, 10, 9, 2, 12, 5, 18, 5, 1, 17]) == False)
  , ((checkAnySize 10 [12, 14, 15, 13, 6, 2, 4, 6, 8]) == False)
  , ((checkAnySize 1 [1]) == True)
  , ((checkAnySize 19 [1]) == False)
  , ((checkAnySize 19 [10, 18]) == False)
  , ((checkAnySize 0 [1]) == True)
  ]
testLinesString =
  [ "(radius 18.021375982802088 173.33510724369103)"
  , "(radius 0 412.32913662440524)"
  , "(radius 0 0)"
  , "(radius 67.93296283710589 0)"
  , "(radius 0.5440756915691 0.2303903996966)"
  , "(radius (-311.34567118802806) (-240.61110395188845))"
  , "(radius (-212.73760404278133) 208.10481117806063)"
  , "(radius 470.09681510124034 (-269.8274333824833))"
  , "(radius 0.3987925468672694 3.547137081723859)"
  , "(radius 150.58107146415188 214.24393399315437)"
  , "(sumEvens 0)"
  , "(sumEvens 1)"
  , "(sumEvens 5)"
  , "(sumEvens 7)"
  , "(sumEvens 10)"
  , "(sumEvens 1000)"
  , "(sumEvens 12010)"
  , "(sumEvens 21391)"
  , "(sumEvens 11987)"
  , "(sumEvens 20011)"
  , "(multiplyEnds [])"
  , "(multiplyEnds [0])"
  , "(multiplyEnds [10])"
  , "(multiplyEnds [1, 2, 3])"
  , "(multiplyEnds [40593, 27417])"
  , "(multiplyEnds [35149, 87651, 35911, 83397, 99370])"
  , "(multiplyEnds [38420, 40661, 13332, 8795])"
  , "(multiplyEnds [43593, 14552, 3952, 90453, 40925, 85543])"
  , "(multiplyEnds [299, 68816, 9249, 13903, 71450, 92730, 98639, 86378])"
  , "(multiplyEnds [98030, 43375, 13656, 31539, 60734, 30991, 1092])"
  , "(getLengths [])"
  , "(getLengths [\"hello\", \"world\"])"
  , "(getLengths [\"\", \"\", \"\", \"a\"])"
  , "(getLengths [\"lorem\", \"ipsum\", \"dolor\", \"sit\", \"amet\"])"
  , "(getLengths [\"1\", \"12\", \"123\", \"1234\", \"12345\", \"123456\", \"1234567\"])"
  , "(getLengths [\"[]\", \"[][][]\", \"[][\", \"[[[]]]\"])"
  , "(getLengths [\"[qn5MY6(dH\", \"dmGiRCExcA8CODW6pKIXU5\", \"z3RVBj\"])"
  , "(getLengths [\"6RXim2_mGXbb]VEWU\", \"6LS]U_3JZuaaCRM2C\", \"z\", \"ySNI(\", \"oc[6sBBvoeIC7CXTc[kKuY\", \"\", \"tARQnW\", \"B\", \"uDB_6CsE4zk9dLN\", \"CZNRMZlTdiktBltJG125\"])"
  , "(getLengths [\"((H5groMY[V-\", \"B3K\", \"3L]ECyVEgyBZ\", \"_H[6lLX-\", \"Y3W3]h-l\", \"Y4bWF_]7wZmJh\", \"DDvnAOc\", \"-bV9AS7y5bn\"])"
  , "(getLengths [\"wFJeZ7bk9C2KQuM9\", \"9zM4PI[\", \"H8sbR_FQqm(\", \"oCiBL\", \"a]WpuXJExZ\", \"i3l-q6wOsq9RGG]UbAr\", \"gKUK4CLNLycHOjt\", \"yzW]EAGeVIx6W-\", \"(1Jsqgc9\"])"
  , "(dropLastTwo [23505, 57977])"
  , "(dropLastTwo [95098, 23505])"
  , "(dropLastTwo [55239, 23808, 23505])"
  , "(dropLastTwo [13830, 45840, 96462])"
  , "(dropLastTwo [15558, 92120, 51217, 26348])"
  , "(dropLastTwo [27919, 16857, 22233, 51243, 15288])"
  , "(dropLastTwo [3201, 87168, 21085, 45539, 78773, 46677, 98576, 65939, 82261])"
  , "(dropLastTwo [73741, 60411, 95098, 67195, 59542, 42884, 23505, 47216])"
  , "(dropLastTwo [578, 42300, 88438, 74099, 36219, 7526, 46539, 45197, 14057])"
  , "(dropLastTwo [63710, 96256, 57977, 91623, 66038, 25309, 87389, 22091, 82434])"
  , "(findEmpty [\"X\", \"OwsyeGuTB\", \"\", \"EG\"])"
  , "(findEmpty [\"eNWEWRSXN\"])"
  , "(findEmpty [\"Baamslu\", \"cpajmk\", \"pTIizL\"])"
  , "(findEmpty [\"AkUyepwvq\", \"lglAykB\", \"\", \"QX\", \"\", \"sFGplIKrq\", \"Rcb\"])"
  , "(findEmpty [\"IUoHkfB\", \"kO\", \"IYzO\", \"LvSOgVtjx\"])"
  , "(findEmpty [\"\"])"
  , "(findEmpty [\"iNukiw\"])"
  , "(findEmpty [\"\", \"k\", \"VDXuT\", \"dKsaKnJYc\"])"
  , "(findEmpty [\"rdSvhNCj\", \"gyeomNbt\", \"Tg\", \"LQ\", \"xO\", \"HNf\", \"\"])"
  , "(findEmpty [\"s\", \"R\", \"Cr\", \"HcGTPxv\", \"KCvNH\", \"K\", \"VATn\"])"
  , "(checkPalindrome \"aaabbbaaa\")"
  , "(checkPalindrome \"aaabbaaa\")"
  , "(checkPalindrome \"ababa\")"
  , "(checkPalindrome \"a\")"
  , "(checkPalindrome \"aaaaaaaaa\")"
  , "(checkPalindrome \"DIxeLSPiikG\")"
  , "(checkPalindrome \"tVt\")"
  , "(checkPalindrome \"nfhxHktB\")"
  , "(checkPalindrome \"IeIailw\")"
  , "(checkPalindrome \"ycTsOrH\")"
  , "(checkSize [16, 12, 13])"
  , "(checkSize [9, 9])"
  , "(checkSize [6, 19, 7, 10, 15, 1, 2, 9, 6])"
  , "(checkSize [15, 7, 10, 4, 3, 15, 19, 19, 5])"
  , "(checkSize [15, 7, 16])"
  , "(checkSize [20, 2])"
  , "(checkSize [9])"
  , "(checkSize [])"
  , "(checkSize [6, 2, 19, 3, 5, 3, 7])"
  , "(checkSize [17, 16, 14, 11, 14, 17, 15])"
  , "(checkAnySize 2 [19, 16, 7, 1, 18, 5, 6, 17, 12, 14, 7])"
  , "(checkAnySize 0 [0])"
  , "(checkAnySize 12 [12, 15, 10, 10, 10, 6, 14, 9, 9, 13, 5, 8, 5, 11])"
  , "(checkAnySize 18 [11, 4, 1, 7, 14, 14, 7, 14, 7, 16, 6, 17, 15, 15, 18, 15, 18, 11])"
  , "(checkAnySize 9 [6, 10, 9, 2, 12, 5, 18, 5, 1, 17])"
  , "(checkAnySize 10 [12, 14, 15, 13, 6, 2, 4, 6, 8])"
  , "(checkAnySize 1 [1])"
  , "(checkAnySize 19 [1])"
  , "(checkAnySize 19 [10, 18])"
  , "(checkAnySize 0 [1])"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
