insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

sortedEq :: Ord a => [a] -> [a] -> Bool
sortedEq as bs = insertionSort as == insertionSort bs

tests =
  [ ((mapPair take (zip [1..] ["abfsd", "dawda", "dawda", "dawdawd"])) == ["a","da","daw","dawd"])
  , ((mapPair take (zip [1..] ([]::[[Int]]))) == [])
  , ((mapPair drop (zip [1..] ["daw", "DWAVS", "12e"])) == ["aw","AVS",""])
  , ((mapPair replicate (zip [1..] "abcdef")) == ["a","bb","ccc","dddd","eeeee","ffffff"])
  , ((mapPair (\x y -> x * y) (zip [1..5] [6..20])) == [6,14,24,36,50])
  , ((mapPair (\x y -> x `mod` y) (zip [10,22..100] [6..20])) == [4,1,2,1,8,4,10,3])
  , ((mapPair (\x y -> length x > y) (zip ["abcdef", "abcd", "abc", "ab", "a"] [1..])) == [True,True,False,False,False])
  , ((mapPair (\x y -> length x < y) (zip ["abcdef", "abcd", "abc", "ab", "a"] [1..])) == [False,False,False,True,True])
  , ((mapPair (\x y -> fst x `mod` snd y > 0) (zip (zip [5..9] [5..10]) (zip [6..15] [1..5]))) == [False,False,True,False,True])
  , ((mapPair (\x y -> fst x * y) (zip (zip [5..9] [5..10]) [6..15])) == [30,42,56,72,90])
  , ((mapPair' (flip take) (zip [1..] ["abfsd", "dawda", "dawda", "dawdawd"])) == ["a","da","daw","dawd"])
  , ((mapPair' (flip take) (zip [1..] ([]::[[Int]]))) == [])
  , ((mapPair' (flip drop) (zip [1..] ["daw", "DWAVS", "12e"])) == ["aw","AVS",""])
  , ((mapPair' (flip replicate) (zip [1..] "abcdef")) == ["a","bb","ccc","dddd","eeeee","ffffff"])
  , ((mapPair' (flip (\x y -> x * y)) (zip [1..5] [6..20])) == [6,14,24,36,50])
  , ((mapPair' (flip (\x y -> x `mod` y)) (zip [10,22..100] [6..20])) == [4,1,2,1,8,4,10,3])
  , ((mapPair' (flip (\x y -> length x > y)) (zip ["abcdef", "abcd", "abc", "ab", "a"] [1..])) == [True,True,False,False,False])
  , ((mapPair' (flip (\x y -> length x < y)) (zip ["abcdef", "abcd", "abc", "ab", "a"] [1..])) == [False,False,False,True,True])
  , ((mapPair' (flip (\x y -> fst x `mod` snd y > 0)) (zip (zip [5..9] [5..10]) (zip [6..15] [1..5]))) == [False,False,True,False,True])
  , ((mapPair' (flip (\x y -> fst x * y)) (zip (zip [5..9] [5..10]) [6..15])) == [30,42,56,72,90])
  , ((digitsOnly [-9, 14, 13, -10, 14, -9, -8]) == [])
  , ((digitsOnly [-2, -4, 13]) == [])
  , ((digitsOnly [11, -6, -6, 0, 0, 5]) == [0,0,5])
  , ((digitsOnly [13, -9, 5, -1]) == [5])
  , ((digitsOnly [1, 2, 3]) == [1,2,3])
  , ((digitsOnly [5, -1, -9, -6, -3, -2, -2]) == [5])
  , ((digitsOnly [-8, -2, 3, -1, 3, 3, 9, -2, -6]) == [3,3,3,9])
  , ((digitsOnly [-7, 14, -7, -9, -5, 12]) == [])
  , ((digitsOnly [12, 2, -7, -3, -10, -3]) == [2])
  , ((digitsOnly []) == [])
  , ((removeXs ["", "AV", "XRQF", "P"]) == ["","AV","P"])
  , ((removeXs ["MQ", "", ""]) == ["MQ","",""])
  , ((removeXs ["DMS", "", "ZKAG"]) == ["DMS","","ZKAG"])
  , ((removeXs ["NAV", "XNNU", ""]) == ["NAV",""])
  , ((removeXs ["XWRAB", "XCQJ", "XFNBE", "XQVHA"]) == [])
  , ((removeXs ["", "", ""]) == ["","",""])
  , ((removeXs ["AWP", "VQXQ", "A", "XV"]) == ["AWP","VQXQ","A"])
  , ((removeXs ["XAFJM", "RQXKT"]) == ["RQXKT"])
  , ((removeXs ["NS", "DX"]) == ["NS","DX"])
  , ((removeXs ["BAZU", "J"]) == ["BAZU","J"])
  , ((sqLens ["VROYJCI", "", "CZZIJK", ""]) == [49,0,36,0])
  , ((sqLens ["SPOZKJV"]) == [49])
  , ((sqLens ["KBGTWNU", "VKR", "MMW", "JZQUYCTZ", "IWGX", "ERX", "BFRXI"]) == [49,9,9,64,16,9,25])
  , ((sqLens ["PALVY", "WNY", "SP"]) == [25,9,4])
  , ((sqLens ["JFZDLEGE", "ATFEFO", "O", "ABST", "PAVJR", "RKSYRHGNU", "Y"]) == [64,36,1,16,25,81,1])
  , ((sqLens [""]) == [0])
  , ((sqLens []) == [])
  , ((sqLens ["YEZ", "EQR", "NXLYSVX", "ELY", "KETXKMFXN", "O", "E"]) == [9,9,49,9,81,1,1])
  , ((sqLens ["CBJUZJ", "GMCWOZ", "CFYQVQGM", "IKMF"]) == [36,36,64,16])
  , ((sqLens ["YZ", "KCWJH", "E", "WFJPYA", "OBETFCLA", "M", "F"]) == [4,25,1,36,64,1,1])
  , ((bang ["VROYJCI", "", "CZZIJK", ""]) == ["VROYJCI!","!","CZZIJK!","!"])
  , ((bang ["SPOZKJV"]) == ["SPOZKJV!"])
  , ((bang ["KBGTWNU", "VKR", "MMW", "JZQUYCTZ", "IWGX", "ERX", "BFRXI"]) == ["KBGTWNU!","VKR!","MMW!","JZQUYCTZ!","IWGX!","ERX!","BFRXI!"])
  , ((bang ["PALVY", "WNY", "SP"]) == ["PALVY!","WNY!","SP!"])
  , ((bang ["JFZDLEGE", "ATFEFO", "O", "ABST", "PAVJR", "RKSYRHGNU", "Y"]) == ["JFZDLEGE!","ATFEFO!","O!","ABST!","PAVJR!","RKSYRHGNU!","Y!"])
  , ((bang [""]) == ["!"])
  , ((bang ([]::[String])) == [])
  , ((bang ["YEZ", "EQR", "NXLYSVX", "ELY", "KETXKMFXN", "O", "E"]) == ["YEZ!","EQR!","NXLYSVX!","ELY!","KETXKMFXN!","O!","E!"])
  , ((bang ["CBJUZJ", "GMCWOZ", "CFYQVQGM", "IKMF"]) == ["CBJUZJ!","GMCWOZ!","CFYQVQGM!","IKMF!"])
  , ((bang ["YZ", "KCWJH", "E", "WFJPYA", "OBETFCLA", "M", "F"]) == ["YZ!","KCWJH!","E!","WFJPYA!","OBETFCLA!","M!","F!"])
  , ((diff ([]::[Integer]) ([]::[Integer])) == [])
  , ((diff [29, 86] ([]::[Integer])) == [])
  , ((diff ([]::[Integer]) [62, 167, 55, 139]) == [])
  , ((diff [160, -137] [1, 2,3 , 4, 5]) == [159,-139])
  , ((diff [1, 324, 6, 75, 4, 2, 12, 3] [-23, 163, 89, -153]) == [24,161,-83,228])
  , ((diff [38, -58, -117, -30, -98] [124, 23, 14, 534, 23, 1]) == [-86,-81,-131,-564,-121])
  , ((diff [-200] [-103, 135]) == [-97])
  , ((diff [23, 156, -121, -20, -164, -160, 193, 30] [-23, 163, 89, -153]) == [46,-7,-210,133])
  , ((diff [-180] [-200]) == [20])
  , ((diff [130] ([]::[Integer])) == [])
  , ((splice ["KYGGAMG", "XVZVG"] ["WQPFRPQSO", "TCABVRX"]) == ["KYGGAMGWQPFRPQSOKYGGAMG","XVZVGTCABVRXXVZVG"])
  , ((splice ["YBBSG", "A", "HFHRBBQ", "JWN"] ["UEDVGQ", "IMVNXL"]) == ["YBBSGUEDVGQYBBSG","AIMVNXLA"])
  , ((splice ["OJYIC", "MRXHYXR"] ["TSLOZG"]) == ["OJYICTSLOZGOJYIC"])
  , ((splice ["VAWHPPYEH", "ZWHCMPKWO"] ["LYHAFERUA", "CQIR", "AKCXPAK"]) == ["VAWHPPYEHLYHAFERUAVAWHPPYEH","ZWHCMPKWOCQIRZWHCMPKWO"])
  , ((splice ["KBSWEWT", "HUVA", "AXOPV"] ["OGGIY", "JBPHLX"]) == ["KBSWEWTOGGIYKBSWEWT","HUVAJBPHLXHUVA"])
  , ((splice [] []) == [])
  , ((splice ["WHA", "SWVKLMW"] ["FIJNVBSR"]) == ["WHAFIJNVBSRWHA"])
  , ((splice ["LESBEXYB", "QVUKOWEG", "FMFCUI"] ["RWQPSPZ", "NTN", "ZKPBWNV"]) == ["LESBEXYBRWQPSPZLESBEXYB","QVUKOWEGNTNQVUKOWEG","FMFCUIZKPBWNVFMFCUI"])
  , ((splice ["FKMFXRHPD", "QTKODBVLL"] ["NJCUEQAVK", "KENBRTLXH"]) == ["FKMFXRHPDNJCUEQAVKFKMFXRHPD","QTKODBVLLKENBRTLXHQTKODBVLL"])
  , ((splice ["W"] ["AIH"]) == ["WAIHW"])
  , ((firstStop "pacyt.wdzr") == "pacyt")
  , ((firstStop ".n") == "")
  , ((firstStop "mmvpflci") == "mmvpflci")
  , ((firstStop "qeevhlblqq.") == "qeevhlblqq")
  , ((firstStop ".") == "")
  , ((firstStop "") == "")
  , ((firstStop "zfpdn.wwjn") == "zfpdn")
  , ((firstStop "k.setoet") == "k")
  , ((firstStop ".vnomecm") == "")
  , ((firstStop "qfn.oyi") == "qfn")
  , ((boundRange 6 [1..12]) == [1,2,3,4,5,6])
  , ((boundRange 9 [1,-4,-5,6,-10,10,9,-9]) == [1,-4,-5,6])
  , ((boundRange 2 []) == [])
  , ((boundRange 2 [0]) == [0])
  , ((boundRange 0 [1]) == [])
  , ((boundRange 0 []) == [])
  , ((boundRange 4 [1,25,6,7,-9,9,-6,5,-4,3,2,11]) == [1])
  , ((boundRange 1 [-2, 1, 0, -1, 2]) == [])
  , ((boundRange 2 [2, 1, 0, -1]) == [2,1,0,-1])
  , ((boundRange 6 [12, 34, 5, 32, -4, -1, 42, 1, 23, 1, -23, -7, 6]) == [])
  , ((exists even [1,2,3,4,5,6]) == True)
  , ((exists even [6]) == True)
  , ((exists even []) == False)
  , ((exists odd [1,2,3,4,5,6]) == True)
  , ((exists (\x -> reverse x == x) ["madam im adam", "fuss"]) == False)
  , ((exists (\x -> reverse x == x) ["fuss", "aaabbaaa", "floss"]) == True)
  , ((exists (\x -> reverse x == x) ["fuss", "floss", "aaabbaaa"]) == True)
  , ((exists (\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (5 , 7), (3 , 3)]) == False)
  , ((exists (\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (6 , 7), (3 , 3)]) == True)
  , ((exists (\x -> True) []) == False)
  , ((exists' even [1,2,3,4,5,6]) == True)
  , ((exists' even [6]) == True)
  , ((exists' even []) == False)
  , ((exists' odd [1,2,3,4,5,6]) == True)
  , ((exists' (\x -> reverse x == x) ["madam im adam", "fuss"]) == False)
  , ((exists' (\x -> reverse x == x) ["fuss", "aaabbaaa", "floss"]) == True)
  , ((exists' (\x -> reverse x == x) ["fuss", "floss", "aaabbaaa"]) == True)
  , ((exists' (\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (5 , 7), (3 , 3)]) == False)
  , ((exists' (\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (6 , 7), (3 , 3)]) == True)
  , ((exists' (\x -> True) []) == False)
  , ((noDups ([]::[Int])) `sortedEq` [])
  , ((noDups ["snug"]) `sortedEq` ["snug"])
  , ((noDups ["snug", "snug"]) `sortedEq` ["snug"])
  , ((noDups ["snug", "snug", "snug", "shellfish", "snug"]) `sortedEq` ["snug","shellfish"])
  , ((noDups [1, 2, 3, 4, 1, 2, 3, 4, 5]) `sortedEq` [1,2,3,4,5])
  , ((noDups [1, 2, 3, 4]) `sortedEq` [1,2,3,4])
  , ((noDups [('a', 2), ('a', 3), ('a', 2)]) `sortedEq` [('a',2),('a',3)])
  , ((noDups ['a', 'a', 'b', 'c', 'd', 'a', 'a']) `sortedEq` "abcd")
  , ((noDups [6, 4, 3, 3, 4, 5, 4, 5, 6]) `sortedEq` [6,4,3,5])
  , ((noDups [2, 2]) `sortedEq` [2])
  , ((noDups' ([]::[Int])) `sortedEq` [])
  , ((noDups' ["snug"]) `sortedEq` ["snug"])
  , ((noDups' ["snug", "snug"]) `sortedEq` ["snug"])
  , ((noDups' ["snug", "snug", "snug", "shellfish", "snug"]) `sortedEq` ["snug","shellfish"])
  , ((noDups' [1, 2, 3, 4, 1, 2, 3, 4, 5]) `sortedEq` [1,2,3,4,5])
  , ((noDups' [1, 2, 3, 4]) `sortedEq` [1,2,3,4])
  , ((noDups' [('a', 2), ('a', 3), ('a', 2)]) `sortedEq` [('a',2),('a',3)])
  , ((noDups' ['a', 'a', 'b', 'c', 'd', 'a', 'a']) `sortedEq` "abcd")
  , ((noDups' [6, 4, 3, 3, 4, 5, 4, 5, 6]) `sortedEq` [6,4,3,5])
  , ((noDups' [2, 2]) `sortedEq` [2])
  , ((countOverflow 3 ["NGNZELUV", "JSJRFKWD", "UPN", "SRONOABCF"]) == 3)
  , ((countOverflow 4 ["WUJUM", "FXMJOWFD", "PEHJPHF", "SZBLVWUV"]) == 4)
  , ((countOverflow 5 ["IINB", "QNIK", "JTID"]) == 0)
  , ((countOverflow 6 ["K", "GMSV", "KRZFY", "FLVBNYWEH", "WY"]) == 1)
  , ((countOverflow 1 ["XC", "WG", "NXAZP", "KBFMKBP", "UZR", "FPRFCYOT"]) == 6)
  , ((countOverflow 0 ["VRSI", "CRZ", "SN", "RLFZ", "YWS", "UNV"]) == 6)
  , ((countOverflow 10 ["FASFRGPK", "OJ", "ZPMCH", "ZJLGCL", "TYFZABJD", "XXNMGPN"]) == 0)
  , ((countOverflow 9 ["BFQOC", "JYGYUIHBE", "PKAND", "UHXIN"]) == 0)
  , ((countOverflow 4 ["PDL", "AJUL", "PWA", "KVHEFS", "ZEIEANUH", "JSQLNQY"]) == 3)
  , ((countOverflow 3 ["ZARX", "QK", "DMJ", "IXBBWJTR"]) == 2)
  , ((countOverflow' 3 ["NGNZELUV", "JSJRFKWD", "UPN", "SRONOABCF"]) == 3)
  , ((countOverflow' 4 ["WUJUM", "FXMJOWFD", "PEHJPHF", "SZBLVWUV"]) == 4)
  , ((countOverflow' 5 ["IINB", "QNIK", "JTID"]) == 0)
  , ((countOverflow' 6 ["K", "GMSV", "KRZFY", "FLVBNYWEH", "WY"]) == 1)
  , ((countOverflow' 1 ["XC", "WG", "NXAZP", "KBFMKBP", "UZR", "FPRFCYOT"]) == 6)
  , ((countOverflow' 0 ["VRSI", "CRZ", "SN", "RLFZ", "YWS", "UNV"]) == 6)
  , ((countOverflow' 10 ["FASFRGPK", "OJ", "ZPMCH", "ZJLGCL", "TYFZABJD", "XXNMGPN"]) == 0)
  , ((countOverflow' 9 ["BFQOC", "JYGYUIHBE", "PKAND", "UHXIN"]) == 0)
  , ((countOverflow' 4 ["PDL", "AJUL", "PWA", "KVHEFS", "ZEIEANUH", "JSQLNQY"]) == 3)
  , ((countOverflow' 3 ["ZARX", "QK", "DMJ", "IXBBWJTR"]) == 2)
  , ((concatList [["IYYC", "D", "UI", "ZQON"], [], ["UXS"]]) == ["IYYC","D","UI","ZQON","UXS"])
  , ((concatList [[], ["ZSX", "VOE", "YJKD", "LX"], ["KDL", "CIMH", "BN"], ["E"]]) == ["ZSX","VOE","YJKD","LX","KDL","CIMH","BN","E"])
  , ((concatList [["WD", "NNVN", "EI", "TKLE"]]) == ["WD","NNVN","EI","TKLE"])
  , ((concatList [["S", "KAL", "JBEJ", "KVD"], [], ["DZKU", "O", "W", "GMX"]]) == ["S","KAL","JBEJ","KVD","DZKU","O","W","GMX"])
  , ((concatList [["BYUP", "EKKU"]]) == ["BYUP","EKKU"])
  , ((concatList [([]::[Int])]) == [])
  , ((concatList [[215], [416, 916, 953, 194], [556, 869, 436]]) == [215,416,916,953,194,556,869,436])
  , ((concatList [[127, 462]]) == [127,462])
  , ((concatList [[("NQGV", 237), ("", 523), ("ZD", 118)], [("U", 325), ("A", 670), ("NST", 70)], [], [], [("DWD", 26), ("LZEM", 695)]]) == [("NQGV",237),("",523),("ZD",118),("U",325),("A",670),("NST",70),("DWD",26),("LZEM",695)])
  , ((concatList [[("", 599), ("QQ", 473), ("ORPV", 550)]]) == [("",599),("QQ",473),("ORPV",550)])
  , ((concatList' [["IYYC", "D", "UI", "ZQON"], [], ["UXS"]]) == ["IYYC","D","UI","ZQON","UXS"])
  , ((concatList' [[], ["ZSX", "VOE", "YJKD", "LX"], ["KDL", "CIMH", "BN"], ["E"]]) == ["ZSX","VOE","YJKD","LX","KDL","CIMH","BN","E"])
  , ((concatList' [["WD", "NNVN", "EI", "TKLE"]]) == ["WD","NNVN","EI","TKLE"])
  , ((concatList' [["S", "KAL", "JBEJ", "KVD"], [], ["DZKU", "O", "W", "GMX"]]) == ["S","KAL","JBEJ","KVD","DZKU","O","W","GMX"])
  , ((concatList' [["BYUP", "EKKU"]]) == ["BYUP","EKKU"])
  , ((concatList' [([]::[Int])]) == [])
  , ((concatList' [[215], [416, 916, 953, 194], [556, 869, 436]]) == [215,416,916,953,194,556,869,436])
  , ((concatList' [[127, 462]]) == [127,462])
  , ((concatList' [[("NQGV", 237), ("", 523), ("ZD", 118)], [("U", 325), ("A", 670), ("NST", 70)], [], [], [("DWD", 26), ("LZEM", 695)]]) == [("NQGV",237),("",523),("ZD",118),("U",325),("A",670),("NST",70),("DWD",26),("LZEM",695)])
  , ((concatList' [[("", 599), ("QQ", 473), ("ORPV", 550)]]) == [("",599),("QQ",473),("ORPV",550)])
  , ((bindList (replicate 3) [1,2,3]) == [1,1,1,2,2,2,3,3,3])
  , ((bindList (\x -> [x]) ['a', 'b', 'c', 'd', 'e', 'f']) == "abcdef")
  , ((bindList show [1,2,3,4,5]) == "12345")
  , ((bindList (reverse . show) [1,2,3,4,5]) == "12345")
  , ((bindList (map (^2)) [[1,2,3,4], [5,6,7,8], [9,10,11], [12,13]]) == [1,4,9,16,25,36,49,64,81,100,121,144,169])
  , ((bindList (\x -> [([]::[Int])]) ([]::[Int])) == [])
  , ((bindList (\x -> [([]::[Int])]) [1, 2, 3, 4]) == [[],[],[],[]])
  , ((bindList (\x -> ([]::[Int])) [1,2,3,4,5]) == [])
  , ((bindList (\x -> [x]) ([]::[Int])) == [])
  , ((bindList (\x -> [(x,'A'), (x,'B')]) [1,2,3,4,54,5,6,65,784,673,54,5]) == [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B'),(54,'A'),(54,'B'),(5,'A'),(5,'B'),(6,'A'),(6,'B'),(65,'A'),(65,'B'),(784,'A'),(784,'B'),(673,'A'),(673,'B'),(54,'A'),(54,'B'),(5,'A'),(5,'B')])
  , ((bindList' (replicate 3) [1,2,3]) == [1,1,1,2,2,2,3,3,3])
  , ((bindList' (\x -> [x]) ['a', 'b', 'c', 'd', 'e', 'f']) == "abcdef")
  , ((bindList' show [1,2,3,4,5]) == "12345")
  , ((bindList' (reverse . show) [1,2,3,4,5]) == "12345")
  , ((bindList' (map (^2)) [[1,2,3,4], [5,6,7,8], [9,10,11], [12,13]]) == [1,4,9,16,25,36,49,64,81,100,121,144,169])
  , ((bindList' (\x -> [([]::[Int])]) ([]::[Int])) == [])
  , ((bindList' (\x -> [([]::[Int])]) [1, 2, 3, 4]) == [[],[],[],[]])
  , ((bindList' (\x -> ([]::[Int])) [1,2,3,4,5]) == [])
  , ((bindList' (\x -> [x]) ([]::[Int])) == [])
  , ((bindList' (\x -> [(x,'A'), (x,'B')]) [1,2,3,4,54,5,6,65,784,673,54,5]) == [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B'),(54,'A'),(54,'B'),(5,'A'),(5,'B'),(6,'A'),(6,'B'),(65,'A'),(65,'B'),(784,'A'),(784,'B'),(673,'A'),(673,'B'),(54,'A'),(54,'B'),(5,'A'),(5,'B')])
  ]
testLinesString =
  [ "(mapPair take (zip [1..] [\"abfsd\", \"dawda\", \"dawda\", \"dawdawd\"]))"
  , "(mapPair take (zip [1..] ([]::[[Int]])))"
  , "(mapPair drop (zip [1..] [\"daw\", \"DWAVS\", \"12e\"]))"
  , "(mapPair replicate (zip [1..] \"abcdef\"))"
  , "(mapPair (\\x y -> x * y) (zip [1..5] [6..20]))"
  , "(mapPair (\\x y -> x `mod` y) (zip [10,22..100] [6..20]))"
  , "(mapPair (\\x y -> length x > y) (zip [\"abcdef\", \"abcd\", \"abc\", \"ab\", \"a\"] [1..]))"
  , "(mapPair (\\x y -> length x < y) (zip [\"abcdef\", \"abcd\", \"abc\", \"ab\", \"a\"] [1..]))"
  , "(mapPair (\\x y -> fst x `mod` snd y > 0) (zip (zip [5..9] [5..10]) (zip [6..15] [1..5])))"
  , "(mapPair (\\x y -> fst x * y) (zip (zip [5..9] [5..10]) [6..15]))"
  , "(mapPair' (flip take) (zip [1..] [\"abfsd\", \"dawda\", \"dawda\", \"dawdawd\"]))"
  , "(mapPair' (flip take) (zip [1..] ([]::[[Int]])))"
  , "(mapPair' (flip drop) (zip [1..] [\"daw\", \"DWAVS\", \"12e\"]))"
  , "(mapPair' (flip replicate) (zip [1..] \"abcdef\"))"
  , "(mapPair' (flip (\\x y -> x * y)) (zip [1..5] [6..20]))"
  , "(mapPair' (flip (\\x y -> x `mod` y)) (zip [10,22..100] [6..20]))"
  , "(mapPair' (flip (\\x y -> length x > y)) (zip [\"abcdef\", \"abcd\", \"abc\", \"ab\", \"a\"] [1..]))"
  , "(mapPair' (flip (\\x y -> length x < y)) (zip [\"abcdef\", \"abcd\", \"abc\", \"ab\", \"a\"] [1..]))"
  , "(mapPair' (flip (\\x y -> fst x `mod` snd y > 0)) (zip (zip [5..9] [5..10]) (zip [6..15] [1..5])))"
  , "(mapPair' (flip (\\x y -> fst x * y)) (zip (zip [5..9] [5..10]) [6..15]))"
  , "(digitsOnly [-9, 14, 13, -10, 14, -9, -8])"
  , "(digitsOnly [-2, -4, 13])"
  , "(digitsOnly [11, -6, -6, 0, 0, 5])"
  , "(digitsOnly [13, -9, 5, -1])"
  , "(digitsOnly [1, 2, 3])"
  , "(digitsOnly [5, -1, -9, -6, -3, -2, -2])"
  , "(digitsOnly [-8, -2, 3, -1, 3, 3, 9, -2, -6])"
  , "(digitsOnly [-7, 14, -7, -9, -5, 12])"
  , "(digitsOnly [12, 2, -7, -3, -10, -3])"
  , "(digitsOnly [])"
  , "(removeXs [\"\", \"AV\", \"XRQF\", \"P\"])"
  , "(removeXs [\"MQ\", \"\", \"\"])"
  , "(removeXs [\"DMS\", \"\", \"ZKAG\"])"
  , "(removeXs [\"NAV\", \"XNNU\", \"\"])"
  , "(removeXs [\"XWRAB\", \"XCQJ\", \"XFNBE\", \"XQVHA\"])"
  , "(removeXs [\"\", \"\", \"\"])"
  , "(removeXs [\"AWP\", \"VQXQ\", \"A\", \"XV\"])"
  , "(removeXs [\"XAFJM\", \"RQXKT\"])"
  , "(removeXs [\"NS\", \"DX\"])"
  , "(removeXs [\"BAZU\", \"J\"])"
  , "(sqLens [\"VROYJCI\", \"\", \"CZZIJK\", \"\"])"
  , "(sqLens [\"SPOZKJV\"])"
  , "(sqLens [\"KBGTWNU\", \"VKR\", \"MMW\", \"JZQUYCTZ\", \"IWGX\", \"ERX\", \"BFRXI\"])"
  , "(sqLens [\"PALVY\", \"WNY\", \"SP\"])"
  , "(sqLens [\"JFZDLEGE\", \"ATFEFO\", \"O\", \"ABST\", \"PAVJR\", \"RKSYRHGNU\", \"Y\"])"
  , "(sqLens [\"\"])"
  , "(sqLens [])"
  , "(sqLens [\"YEZ\", \"EQR\", \"NXLYSVX\", \"ELY\", \"KETXKMFXN\", \"O\", \"E\"])"
  , "(sqLens [\"CBJUZJ\", \"GMCWOZ\", \"CFYQVQGM\", \"IKMF\"])"
  , "(sqLens [\"YZ\", \"KCWJH\", \"E\", \"WFJPYA\", \"OBETFCLA\", \"M\", \"F\"])"
  , "(bang [\"VROYJCI\", \"\", \"CZZIJK\", \"\"])"
  , "(bang [\"SPOZKJV\"])"
  , "(bang [\"KBGTWNU\", \"VKR\", \"MMW\", \"JZQUYCTZ\", \"IWGX\", \"ERX\", \"BFRXI\"])"
  , "(bang [\"PALVY\", \"WNY\", \"SP\"])"
  , "(bang [\"JFZDLEGE\", \"ATFEFO\", \"O\", \"ABST\", \"PAVJR\", \"RKSYRHGNU\", \"Y\"])"
  , "(bang [\"\"])"
  , "(bang ([]::[String]))"
  , "(bang [\"YEZ\", \"EQR\", \"NXLYSVX\", \"ELY\", \"KETXKMFXN\", \"O\", \"E\"])"
  , "(bang [\"CBJUZJ\", \"GMCWOZ\", \"CFYQVQGM\", \"IKMF\"])"
  , "(bang [\"YZ\", \"KCWJH\", \"E\", \"WFJPYA\", \"OBETFCLA\", \"M\", \"F\"])"
  , "(diff ([]::[Integer]) ([]::[Integer]))"
  , "(diff [29, 86] ([]::[Integer]))"
  , "(diff ([]::[Integer]) [62, 167, 55, 139])"
  , "(diff [160, -137] [1, 2,3 , 4, 5])"
  , "(diff [1, 324, 6, 75, 4, 2, 12, 3] [-23, 163, 89, -153])"
  , "(diff [38, -58, -117, -30, -98] [124, 23, 14, 534, 23, 1])"
  , "(diff [-200] [-103, 135])"
  , "(diff [23, 156, -121, -20, -164, -160, 193, 30] [-23, 163, 89, -153])"
  , "(diff [-180] [-200])"
  , "(diff [130] ([]::[Integer]))"
  , "(splice [\"KYGGAMG\", \"XVZVG\"] [\"WQPFRPQSO\", \"TCABVRX\"])"
  , "(splice [\"YBBSG\", \"A\", \"HFHRBBQ\", \"JWN\"] [\"UEDVGQ\", \"IMVNXL\"])"
  , "(splice [\"OJYIC\", \"MRXHYXR\"] [\"TSLOZG\"])"
  , "(splice [\"VAWHPPYEH\", \"ZWHCMPKWO\"] [\"LYHAFERUA\", \"CQIR\", \"AKCXPAK\"])"
  , "(splice [\"KBSWEWT\", \"HUVA\", \"AXOPV\"] [\"OGGIY\", \"JBPHLX\"])"
  , "(splice [] [])"
  , "(splice [\"WHA\", \"SWVKLMW\"] [\"FIJNVBSR\"])"
  , "(splice [\"LESBEXYB\", \"QVUKOWEG\", \"FMFCUI\"] [\"RWQPSPZ\", \"NTN\", \"ZKPBWNV\"])"
  , "(splice [\"FKMFXRHPD\", \"QTKODBVLL\"] [\"NJCUEQAVK\", \"KENBRTLXH\"])"
  , "(splice [\"W\"] [\"AIH\"])"
  , "(firstStop \"pacyt.wdzr\")"
  , "(firstStop \".n\")"
  , "(firstStop \"mmvpflci\")"
  , "(firstStop \"qeevhlblqq.\")"
  , "(firstStop \".\")"
  , "(firstStop \"\")"
  , "(firstStop \"zfpdn.wwjn\")"
  , "(firstStop \"k.setoet\")"
  , "(firstStop \".vnomecm\")"
  , "(firstStop \"qfn.oyi\")"
  , "(boundRange 6 [1..12])"
  , "(boundRange 9 [1,-4,-5,6,-10,10,9,-9])"
  , "(boundRange 2 [])"
  , "(boundRange 2 [0])"
  , "(boundRange 0 [1])"
  , "(boundRange 0 [])"
  , "(boundRange 4 [1,25,6,7,-9,9,-6,5,-4,3,2,11])"
  , "(boundRange 1 [-2, 1, 0, -1, 2])"
  , "(boundRange 2 [2, 1, 0, -1])"
  , "(boundRange 6 [12, 34, 5, 32, -4, -1, 42, 1, 23, 1, -23, -7, 6])"
  , "(exists even [1,2,3,4,5,6])"
  , "(exists even [6])"
  , "(exists even [])"
  , "(exists odd [1,2,3,4,5,6])"
  , "(exists (\\x -> reverse x == x) [\"madam im adam\", \"fuss\"])"
  , "(exists (\\x -> reverse x == x) [\"fuss\", \"aaabbaaa\", \"floss\"])"
  , "(exists (\\x -> reverse x == x) [\"fuss\", \"floss\", \"aaabbaaa\"])"
  , "(exists (\\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (5 , 7), (3 , 3)])"
  , "(exists (\\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (6 , 7), (3 , 3)])"
  , "(exists (\\x -> True) [])"
  , "(exists' even [1,2,3,4,5,6])"
  , "(exists' even [6])"
  , "(exists' even [])"
  , "(exists' odd [1,2,3,4,5,6])"
  , "(exists' (\\x -> reverse x == x) [\"madam im adam\", \"fuss\"])"
  , "(exists' (\\x -> reverse x == x) [\"fuss\", \"aaabbaaa\", \"floss\"])"
  , "(exists' (\\x -> reverse x == x) [\"fuss\", \"floss\", \"aaabbaaa\"])"
  , "(exists' (\\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (5 , 7), (3 , 3)])"
  , "(exists' (\\x -> fst x `mod` 2 == 0) [(1 , 2), (1 , 2), (6 , 7), (3 , 3)])"
  , "(exists' (\\x -> True) [])"
  , "(noDups ([]::[Int]))"
  , "(noDups [\"snug\"])"
  , "(noDups [\"snug\", \"snug\"])"
  , "(noDups [\"snug\", \"snug\", \"snug\", \"shellfish\", \"snug\"])"
  , "(noDups [1, 2, 3, 4, 1, 2, 3, 4, 5])"
  , "(noDups [1, 2, 3, 4])"
  , "(noDups [('a', 2), ('a', 3), ('a', 2)])"
  , "(noDups ['a', 'a', 'b', 'c', 'd', 'a', 'a'])"
  , "(noDups [6, 4, 3, 3, 4, 5, 4, 5, 6])"
  , "(noDups [2, 2])"
  , "(noDups' ([]::[Int]))"
  , "(noDups' [\"snug\"])"
  , "(noDups' [\"snug\", \"snug\"])"
  , "(noDups' [\"snug\", \"snug\", \"snug\", \"shellfish\", \"snug\"])"
  , "(noDups' [1, 2, 3, 4, 1, 2, 3, 4, 5])"
  , "(noDups' [1, 2, 3, 4])"
  , "(noDups' [('a', 2), ('a', 3), ('a', 2)])"
  , "(noDups' ['a', 'a', 'b', 'c', 'd', 'a', 'a'])"
  , "(noDups' [6, 4, 3, 3, 4, 5, 4, 5, 6])"
  , "(noDups' [2, 2])"
  , "(countOverflow 3 [\"NGNZELUV\", \"JSJRFKWD\", \"UPN\", \"SRONOABCF\"])"
  , "(countOverflow 4 [\"WUJUM\", \"FXMJOWFD\", \"PEHJPHF\", \"SZBLVWUV\"])"
  , "(countOverflow 5 [\"IINB\", \"QNIK\", \"JTID\"])"
  , "(countOverflow 6 [\"K\", \"GMSV\", \"KRZFY\", \"FLVBNYWEH\", \"WY\"])"
  , "(countOverflow 1 [\"XC\", \"WG\", \"NXAZP\", \"KBFMKBP\", \"UZR\", \"FPRFCYOT\"])"
  , "(countOverflow 0 [\"VRSI\", \"CRZ\", \"SN\", \"RLFZ\", \"YWS\", \"UNV\"])"
  , "(countOverflow 10 [\"FASFRGPK\", \"OJ\", \"ZPMCH\", \"ZJLGCL\", \"TYFZABJD\", \"XXNMGPN\"])"
  , "(countOverflow 9 [\"BFQOC\", \"JYGYUIHBE\", \"PKAND\", \"UHXIN\"])"
  , "(countOverflow 4 [\"PDL\", \"AJUL\", \"PWA\", \"KVHEFS\", \"ZEIEANUH\", \"JSQLNQY\"])"
  , "(countOverflow 3 [\"ZARX\", \"QK\", \"DMJ\", \"IXBBWJTR\"])"
  , "(countOverflow' 3 [\"NGNZELUV\", \"JSJRFKWD\", \"UPN\", \"SRONOABCF\"])"
  , "(countOverflow' 4 [\"WUJUM\", \"FXMJOWFD\", \"PEHJPHF\", \"SZBLVWUV\"])"
  , "(countOverflow' 5 [\"IINB\", \"QNIK\", \"JTID\"])"
  , "(countOverflow' 6 [\"K\", \"GMSV\", \"KRZFY\", \"FLVBNYWEH\", \"WY\"])"
  , "(countOverflow' 1 [\"XC\", \"WG\", \"NXAZP\", \"KBFMKBP\", \"UZR\", \"FPRFCYOT\"])"
  , "(countOverflow' 0 [\"VRSI\", \"CRZ\", \"SN\", \"RLFZ\", \"YWS\", \"UNV\"])"
  , "(countOverflow' 10 [\"FASFRGPK\", \"OJ\", \"ZPMCH\", \"ZJLGCL\", \"TYFZABJD\", \"XXNMGPN\"])"
  , "(countOverflow' 9 [\"BFQOC\", \"JYGYUIHBE\", \"PKAND\", \"UHXIN\"])"
  , "(countOverflow' 4 [\"PDL\", \"AJUL\", \"PWA\", \"KVHEFS\", \"ZEIEANUH\", \"JSQLNQY\"])"
  , "(countOverflow' 3 [\"ZARX\", \"QK\", \"DMJ\", \"IXBBWJTR\"])"
  , "(concatList [[\"IYYC\", \"D\", \"UI\", \"ZQON\"], [], [\"UXS\"]])"
  , "(concatList [[], [\"ZSX\", \"VOE\", \"YJKD\", \"LX\"], [\"KDL\", \"CIMH\", \"BN\"], [\"E\"]])"
  , "(concatList [[\"WD\", \"NNVN\", \"EI\", \"TKLE\"]])"
  , "(concatList [[\"S\", \"KAL\", \"JBEJ\", \"KVD\"], [], [\"DZKU\", \"O\", \"W\", \"GMX\"]])"
  , "(concatList [[\"BYUP\", \"EKKU\"]])"
  , "(concatList [([]::[Int])])"
  , "(concatList [[215], [416, 916, 953, 194], [556, 869, 436]])"
  , "(concatList [[127, 462]])"
  , "(concatList [[(\"NQGV\", 237), (\"\", 523), (\"ZD\", 118)], [(\"U\", 325), (\"A\", 670), (\"NST\", 70)], [], [], [(\"DWD\", 26), (\"LZEM\", 695)]])"
  , "(concatList [[(\"\", 599), (\"QQ\", 473), (\"ORPV\", 550)]])"
  , "(concatList' [[\"IYYC\", \"D\", \"UI\", \"ZQON\"], [], [\"UXS\"]])"
  , "(concatList' [[], [\"ZSX\", \"VOE\", \"YJKD\", \"LX\"], [\"KDL\", \"CIMH\", \"BN\"], [\"E\"]])"
  , "(concatList' [[\"WD\", \"NNVN\", \"EI\", \"TKLE\"]])"
  , "(concatList' [[\"S\", \"KAL\", \"JBEJ\", \"KVD\"], [], [\"DZKU\", \"O\", \"W\", \"GMX\"]])"
  , "(concatList' [[\"BYUP\", \"EKKU\"]])"
  , "(concatList' [([]::[Int])])"
  , "(concatList' [[215], [416, 916, 953, 194], [556, 869, 436]])"
  , "(concatList' [[127, 462]])"
  , "(concatList' [[(\"NQGV\", 237), (\"\", 523), (\"ZD\", 118)], [(\"U\", 325), (\"A\", 670), (\"NST\", 70)], [], [], [(\"DWD\", 26), (\"LZEM\", 695)]])"
  , "(concatList' [[(\"\", 599), (\"QQ\", 473), (\"ORPV\", 550)]])"
  , "(bindList (replicate 3) [1,2,3])"
  , "(bindList (\\x -> [x]) ['a', 'b', 'c', 'd', 'e', 'f'])"
  , "(bindList show [1,2,3,4,5])"
  , "(bindList (reverse . show) [1,2,3,4,5])"
  , "(bindList (map (^2)) [[1,2,3,4], [5,6,7,8], [9,10,11], [12,13]])"
  , "(bindList (\\x -> [([]::[Int])]) ([]::[Int]))"
  , "(bindList (\\x -> [([]::[Int])]) [1, 2, 3, 4])"
  , "(bindList (\\x -> ([]::[Int])) [1,2,3,4,5])"
  , "(bindList (\\x -> [x]) ([]::[Int]))"
  , "(bindList (\\x -> [(x,'A'), (x,'B')]) [1,2,3,4,54,5,6,65,784,673,54,5])"
  , "(bindList' (replicate 3) [1,2,3])"
  , "(bindList' (\\x -> [x]) ['a', 'b', 'c', 'd', 'e', 'f'])"
  , "(bindList' show [1,2,3,4,5])"
  , "(bindList' (reverse . show) [1,2,3,4,5])"
  , "(bindList' (map (^2)) [[1,2,3,4], [5,6,7,8], [9,10,11], [12,13]])"
  , "(bindList' (\\x -> [([]::[Int])]) ([]::[Int]))"
  , "(bindList' (\\x -> [([]::[Int])]) [1, 2, 3, 4])"
  , "(bindList' (\\x -> ([]::[Int])) [1,2,3,4,5])"
  , "(bindList' (\\x -> [x]) ([]::[Int]))"
  , "(bindList' (\\x -> [(x,'A'), (x,'B')]) [1,2,3,4,54,5,6,65,784,673,54,5])"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
