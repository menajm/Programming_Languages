subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (flip elem ys) xs

eq :: (Eq a) => [a] -> [a] -> Bool
eq xs ys = subset xs ys && subset ys xs

tests =
  [ ((bubble [1..6]) == [1,2,3,4,5,6])
  , ((bubble [0,0,2,5,3,4]) == [0,0,2,3,4,5])
  , ((bubble [6,4,3,2,1,0]) == [4,3,2,1,0,6])
  , ((bubble [11]) == [11])
  , ((bubble ["fork","bowl","spoon"]) == ["bowl","fork","spoon"])
  , ((bubble ["fork","spoon","bowl"]) == ["fork","bowl","spoon"])
  , ((bubble ["fork","bowl","spoon","ada"]) == ["bowl","fork","ada","spoon"])
  , ((bubble [(0,2),(4,6),(1,2),(2,3)]) == [(0,2),(1,2),(2,3),(4,6)])
  , ((bubble [(2,2),(1,1),(4,5),(2,2)]) == [(1,1),(2,2),(2,2),(4,5)])
  , ((bubble ["bowl"]) == ["bowl"])
  , ((bubbleSort [1..6]) == [1,2,3,4,5,6])
  , ((bubbleSort [0,0,2,5,3,4]) == [0,0,2,3,4,5])
  , ((bubbleSort [6,4,3,2,1,0]) == [0,1,2,3,4,6])
  , ((bubbleSort [10]) == [10])
  , ((bubbleSort ["fork","bowl","spoon"]) == ["bowl","fork","spoon"])
  , ((bubbleSort ["fork","spoon","bowl"]) == ["bowl","fork","spoon"])
  , ((bubbleSort ["fork","bowl","spoon","ada"]) == ["ada","bowl","fork","spoon"])
  , ((bubbleSort [(0,2),(4,6),(1,2),(2,3)]) == [(0,2),(1,2),(2,3),(4,6)])
  , ((bubbleSort [(2,2),(1,1),(4,5),(2,2)]) == [(1,1),(2,2),(2,2),(4,5)])
  , ((bubbleSort ["bowl"]) == ["bowl"])
  , ((isSubstring "" "") == True)
  , ((isSubstring "" "goodbye") == True)
  , ((isSubstring "bye" "") == False)
  , ((isSubstring "hello" "hello world") == True)
  , ((isSubstring "bc" "abcdef") == True)
  , ((isSubstring "bye" "goodbye") == True)
  , ((isSubstring " " "") == False)
  , ((isSubstring " " "good bye") == True)
  , ((isSubstring "coal" "coral") == False)
  , ((isSubstring "he" "sheep") == True)
  , ((genPrefix "") `eq` [])
  , ((genPrefix "abcd") `eq` ["a","ab","abc","abcd"])
  , ((genPrefix "hello") `eq` ["h","he","hel","hell","hello"])
  , ((genPrefix "goodbye") `eq` ["g","go","goo","good","goodb","goodby","goodbye"])
  , ((genPrefix "haskell") `eq` ["h","ha","has","hask","haske","haskel","haskell"])
  , ((genPrefix "hello world") `eq` ["h","he","hel","hell","hello","hello ","hello w","hello wo","hello wor","hello worl","hello world"])
  , ((genPrefix "ababcd") `eq` ["a","ab","aba","abab","ababc","ababcd"])
  , ((genPrefix "abbacd") `eq` ["a","ab","abb","abba","abbac","abbacd"])
  , ((genPrefix "hello hello") `eq` ["h","he","hel","hell","hello","hello ","hello h","hello he","hello hel","hello hell","hello hello"])
  , ((genPrefix "Apple app") `eq` ["A","Ap","App","Appl","Apple","Apple ","Apple a","Apple ap","Apple app"])
  , ((genSubstrings "") `eq` [""])
  , ((genSubstrings "defg") `eq` ["d","de","def","defg","e","ef","efg","f","fg","g",""])
  , ((genSubstrings "hey") `eq` ["h","he","hey","e","ey","y",""])
  , ((genSubstrings "goodbye") `eq` ["g","go","goo","good","goodb","goodby","goodbye","o","oo","ood","oodb","oodby","oodbye","o","od","odb","odby","odbye","d","db","dby","dbye","b","by","bye","y","ye","e",""])
  , ((genSubstrings "haskell") `eq` ["h","ha","has","hask","haske","haskel","haskell","a","as","ask","aske","askel","askell","s","sk","ske","skel","skell","k","ke","kel","kell","e","el","ell","l","ll","l",""])
  , ((genSubstrings "hello world") `eq` ["h","he","hel","hell","hello","hello ","hello w","hello wo","hello wor","hello worl","hello world","e","el","ell","ello","ello ","ello w","ello wo","ello wor","ello worl","ello world","l","ll","llo","llo ","llo w","llo wo","llo wor","llo worl","llo world","l","lo","lo ","lo w","lo wo","lo wor","lo worl","lo world","o","o ","o w","o wo","o wor","o worl","o world"," "," w"," wo"," wor"," worl"," world","w","wo","wor","worl","world","o","or","orl","orld","r","rl","rld","l","ld","d",""])
  , ((genSubstrings "abasdasd") `eq` ["a","ab","aba","abas","abasd","abasda","abasdas","abasdasd","b","ba","bas","basd","basda","basdas","basdasd","a","as","asd","asda","asdas","asdasd","s","sd","sda","sdas","sdasd","d","da","das","dasd","a","as","asd","s","sd","d",""])
  , ((genSubstrings "asdgasdrwer") `eq` ["a","as","asd","asdg","asdga","asdgas","asdgasd","asdgasdr","asdgasdrw","asdgasdrwe","asdgasdrwer","s","sd","sdg","sdga","sdgas","sdgasd","sdgasdr","sdgasdrw","sdgasdrwe","sdgasdrwer","d","dg","dga","dgas","dgasd","dgasdr","dgasdrw","dgasdrwe","dgasdrwer","g","ga","gas","gasd","gasdr","gasdrw","gasdrwe","gasdrwer","a","as","asd","asdr","asdrw","asdrwe","asdrwer","s","sd","sdr","sdrw","sdrwe","sdrwer","d","dr","drw","drwe","drwer","r","rw","rwe","rwer","w","we","wer","e","er","r",""])
  , ((genSubstrings "sadfsad fsadf") `eq` ["s","sa","sad","sadf","sadfs","sadfsa","sadfsad","sadfsad ","sadfsad f","sadfsad fs","sadfsad fsa","sadfsad fsad","sadfsad fsadf","a","ad","adf","adfs","adfsa","adfsad","adfsad ","adfsad f","adfsad fs","adfsad fsa","adfsad fsad","adfsad fsadf","d","df","dfs","dfsa","dfsad","dfsad ","dfsad f","dfsad fs","dfsad fsa","dfsad fsad","dfsad fsadf","f","fs","fsa","fsad","fsad ","fsad f","fsad fs","fsad fsa","fsad fsad","fsad fsadf","s","sa","sad","sad ","sad f","sad fs","sad fsa","sad fsad","sad fsadf","a","ad","ad ","ad f","ad fs","ad fsa","ad fsad","ad fsadf","d","d ","d f","d fs","d fsa","d fsad","d fsadf"," "," f"," fs"," fsa"," fsad"," fsadf","f","fs","fsa","fsad","fsadf","s","sa","sad","sadf","a","ad","adf","d","df","f",""])
  , ((genSubstrings "door indoor") `eq` ["d","do","doo","door","door ","door i","door in","door ind","door indo","door indoo","door indoor","o","oo","oor","oor ","oor i","oor in","oor ind","oor indo","oor indoo","oor indoor","o","or","or ","or i","or in","or ind","or indo","or indoo","or indoor","r","r ","r i","r in","r ind","r indo","r indoo","r indoor"," "," i"," in"," ind"," indo"," indoo"," indoor","i","in","ind","indo","indoo","indoor","n","nd","ndo","ndoo","ndoor","d","do","doo","door","o","oo","oor","o","or","r",""])
  , ((replacePrefix ("no","fun") "no") == "fun")
  , ((replacePrefix ("c","func") "ction") == "function")
  , ((replacePrefix ("f","ref") "factor") == "refactor")
  , ((replacePrefix ("un","re") "unmoved") == "removed")
  , ((replacePrefix ("hel","jel") "hello") == "jello")
  , ((replacePrefix ("know","") "knowledge") == "ledge")
  , ((replacePrefix ("un","") "undo") == "do")
  , ((replacePrefix ("bel","pil") "bell") == "pill")
  , ((replacePrefix ("pow","tow") "power") == "tower")
  , ((replacePrefix ("ton","be") "toner") == "beer")
  , ((replaceString ("no","fun") "") == "")
  , ((replaceString ("send","help") "") == "")
  , ((replaceString ("hel","jel") "hello") == "jello")
  , ((replaceString ("k","king") "work") == "working")
  , ((replaceString ("a","asd") "abasdf") == "asdbasdf")
  , ((replaceString ("a","bg") "absdfsa") == "bgbsdfsa")
  , ((replaceString ("one","hei") "toner") == "their")
  , ((replaceString ("pow","tow") "lower") == "lower")
  , ((replaceString ("atl","") "atlatl") == "atl")
  , ((replaceString ("ton","won") "tonton") == "wonton")
  , ((lookUp 'a' [('a','b'), ('c','y'), ('d','x')]) == 'b')
  , ((lookUp 'c' [('x','x'), ('c','y')]) == 'y')
  , ((lookUp 'c' [('a','z'), ('c','x'), ('b','y')]) == 'x')
  , ((lookUp 'c' [('a','z'), ('b','y'), ('c','x')]) == 'x')
  , ((lookUp 'b' [('a','z'), ('b','y'), ('c','x')]) == 'y')
  , ((lookUp 'z' [('a','z'), ('z','b')]) == 'b')
  , ((lookUp 'h' [(x,x) | x <- ['a'..'z']]) == 'h')
  , ((lookUp 'u' [x | x <- zip ['a'..'z'] (drop 6 (cycle ['a'..'z']))]) == 'a')
  , ((lookUp 'h' [('l','h'),('h','l')]) == 'l')
  , ((lookUp 'l' [('l','h'),('h','l')]) == 'h')
  , ((encode [(x,x) | x <- ['a'..'z']] "hello") == "hello")
  , ((encode [('a','z'),('i','k'),('j','j'),('k','i')] "ijk") == "kji")
  , ((encode [('a','1'), ('b','2'), ('c','3')] "abc") == "123")
  , ((encode [('e','i'), ('l','p'), ('h','q'),('o','a')] "hello") == "qippa")
  , ((encode [('b','y'), ('c','z'), ('a','z')] "abc") == "zyz")
  , ((encode [x | x <- zip ['a'..'z'] (drop 6 (cycle ['a'..'z']))] "hello") == "nkrru")
  , ((encode [('1','4'),('2','5'),('3','6'),('a','d'), ('b','e'), ('c','f')] "abc123") == "def456")
  , ((encode [('1','a'),('2','b'),('3','c'),('a','1'), ('b','2'), ('c','3')] "abc123") == "123abc")
  , ((encode [('1','a'),('2','b'),('3','c'),('a','1'), ('b','2'), ('c','3')] "abc123abc") == "123abc123")
  , ((encode [(x,x) | x <- ['a'..'z']] "") == "")
  , ((makeTable "2" "b") == [('2','b')])
  , ((makeTable "bn" "cd") == [('b','c'),('n','d')])
  , ((makeTable "efgh" "abcd") == [('e','a'),('f','b'),('g','c'),('h','d')])
  , ((makeTable "qrst" "xyzw" ) == [('q','x'),('r','y'),('s','z'),('t','w')])
  , ((makeTable "xyzw" "abcd") == [('x','a'),('y','b'),('z','c'),('w','d')])
  , ((makeTable ['a'..'j'] ['0'..'9']) == zip ['a'..'j'] ['0'..'9'])
  , ((makeTable "abcd" "1234") == [('a','1'),('b','2'),('c','3'),('d','4')])
  , ((makeTable "1234" "abcd") == [('1','a'),('2','b'),('3','c'),('4','d')])
  , ((makeTable "12345" "67890") == [('1','6'),('2','7'),('3','8'),('4','9'),('5','0')])
  , ((makeTable "01" "10") == [('0','1'),('1','0')])
  ]

--
testLinesString =
  [ "(bubble [1..6])"
  , "(bubble [0,0,2,5,3,4])"
  , "(bubble [6,4,3,2,1,0])"
  , "(bubble [11])"
  , "(bubble [\"fork\",\"bowl\",\"spoon\"])"
  , "(bubble [\"fork\",\"spoon\",\"bowl\"])"
  , "(bubble [\"fork\",\"bowl\",\"spoon\",\"ada\"])"
  , "(bubble [(0,2),(4,6),(1,2),(2,3)])"
  , "(bubble [(2,2),(1,1),(4,5),(2,2)])"
  , "(bubble [\"bowl\"])"
  , "(bubbleSort [1..6])"
  , "(bubbleSort [0,0,2,5,3,4])"
  , "(bubbleSort [6,4,3,2,1,0])"
  , "(bubbleSort [10])"
  , "(bubbleSort [\"fork\",\"bowl\",\"spoon\"])"
  , "(bubbleSort [\"fork\",\"spoon\",\"bowl\"])"
  , "(bubbleSort [\"fork\",\"bowl\",\"spoon\",\"ada\"])"
  , "(bubbleSort [(0,2),(4,6),(1,2),(2,3)])"
  , "(bubbleSort [(2,2),(1,1),(4,5),(2,2)])"
  , "(bubbleSort [\"bowl\"])"
  , "(isSubstring \"\" \"\")"
  , "(isSubstring \"\" \"goodbye\")"
  , "(isSubstring \"bye\" \"\")"
  , "(isSubstring \"hello\" \"hello world\")"
  , "(isSubstring \"bc\" \"abcdef\")"
  , "(isSubstring \"bye\" \"goodbye\")"
  , "(isSubstring \" \" \"\")"
  , "(isSubstring \" \" \"good bye\")"
  , "(isSubstring \"coal\" \"coral\")"
  , "(isSubstring \"he\" \"sheep\")"
  , "(genPrefix \"\")"
  , "(genPrefix \"abcd\")"
  , "(genPrefix \"hello\")"
  , "(genPrefix \"goodbye\")"
  , "(genPrefix \"haskell\")"
  , "(genPrefix \"hello world\")"
  , "(genPrefix \"ababcd\")"
  , "(genPrefix \"abbacd\")"
  , "(genPrefix \"hello hello\")"
  , "(genPrefix \"Apple app\")"
  , "(genSubstrings \"\")"
  , "(genSubstrings \"defg\")"
  , "(genSubstrings \"hey\")"
  , "(genSubstrings \"goodbye\")"
  , "(genSubstrings \"haskell\")"
  , "(genSubstrings \"hello world\")"
  , "(genSubstrings \"abasdasd\")"
  , "(genSubstrings \"asdgasdrwer\")"
  , "(genSubstrings \"sadfsad fsadf\")"
  , "(genSubstrings \"door indoor\")"
  , "(replacePrefix (\"no\",\"fun\") \"no\")"
  , "(replacePrefix (\"c\",\"func\") \"ction\")"
  , "(replacePrefix (\"f\",\"ref\") \"factor\")"
  , "(replacePrefix (\"un\",\"re\") \"unmoved\")"
  , "(replacePrefix (\"hel\",\"jel\") \"hello\")"
  , "(replacePrefix (\"know\",\"\") \"knowledge\")"
  , "(replacePrefix (\"un\",\"\") \"undo\")"
  , "(replacePrefix (\"bel\",\"pil\") \"bell\")"
  , "(replacePrefix (\"pow\",\"tow\") \"lower\")"
  , "(replacePrefix (\"one\",\"hei\") \"toner\")"
  , "(replaceString (\"no\",\"fun\") \"\")"
  , "(replaceString (\"send\",\"help\") \"\")"
  , "(replaceString (\"hel\",\"jel\") \"hello\")"
  , "(replaceString (\"k\",\"king\") \"work\")"
  , "(replaceString (\"a\",\"asd\") \"abasdf\")"
  , "(replaceString (\"a\",\"bg\") \"absdfsa\")"
  , "(replaceString (\"one\",\"hei\") \"toner\")"
  , "(replaceString (\"pow\",\"tow\") \"lower\")"
  , "(replaceString (\"atl\",\"\") \"atlatl\")"
  , "(replaceString (\"ton\",\"won\") \"tonton\")"
  , "(lookUp 'a' [('a','b'), ('c','y'), ('d','x')])"
  , "(lookUp 'c' [('x','x'), ('c','y')])"
  , "(lookUp 'c' [('a','z'), ('c','x'), ('b','y')])"
  , "(lookUp 'c' [('a','z'), ('b','y'), ('c','x')])"
  , "(lookUp 'b' [('a','z'), ('b','y'), ('c','x')])"
  , "(lookUp 'z' [('a','z'), ('z','b')])"
  , "(lookUp 'h' [(x,x) | x <- ['a'..'z']])"
  , "(lookUp 'u' [x | x <- zip ['a'..'z'] (drop 6 (cycle ['a'..'z']))])"
  , "(lookUp 'h' [('l','h'),('h','l')])"
  , "(lookUp 'l' [('l','h'),('h','l')])"
  , "(encode [(x,x) | x <- ['a'..'z']] \"hello\")"
  , "(encode [('a','z'),('i','k'),('j','j'),('k','i')] \"ijk\")"
  , "(encode [('a','1'), ('b','2'), ('c','3')] \"abc\")"
  , "(encode [('e','i'), ('l','p'), ('h','q'),('o','a')] \"hello\")"
  , "(encode [('b','y'), ('c','z'), ('a','z')] \"abc\")"
  , "(encode [x | x <- zip ['a'..'z'] (drop 6 (cycle ['a'..'z']))] \"hello\")"
  , "(encode [('1','4'),('2','5'),('3','6'),('a','d'), ('b','e'), ('c','f')] \"abc123\")"
  , "(encode [('1','a'),('2','b'),('3','c'),('a','1'), ('b','2'), ('c','3')] \"abc123\")"
  , "(encode [('1','a'),('2','b'),('3','c'),('a','1'), ('b','2'), ('c','3')] \"abc123abc\")"
  , "(encode [(x,x) | x <- ['a'..'z']] \"\")"
  , "(makeTable \"2\" \"b\")"
  , "(makeTable \"bn\" \"cd\")"
  , "(makeTable \"efgh\" \"abcd\")"
  , "(makeTable \"qrst\" \"xyzw\")"
  , "(makeTable \"xyzw\" \"abcd\")"
  , "(makeTable ['a'..'j'] ['0'..'9'])"
  , "(makeTable \"abcd\" \"1234\")"
  , "(makeTable \"1234\" \"abcd\")"
  , "(makeTable \"12345\" \"67890\")"
  , "(makeTable \"01\" \"10\")"
  ]

runTests = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (putStrLn . snd) (filter (not . fst) zipped))
  return ()
