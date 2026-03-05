-- Data.list and Data.Char will be very helpful when we are doing this
{- int main ()
{ 
    int s = 0; int i;
    for (i = 0; i <= 100; i++)
        sum = sum + i;
    printf("%d", sum);
    return sum;
} -}
-- Binary operators
data BOps = Eq | LTE | GTE | AddOp | SubOp | Gt | Lt | MulOp | DivOp

data Types = IntType | BoolType | CharType | StringType
data Keywords = ForK | ReturnK | IfK | ThenK | ElseK | WhileK
data Token = Type Types | Keyword Keywords
            | VSym String | CSym Integer | FName String
            | LBra | RBra | LPar | RPar | Comma | Semi
            | BOp BOps | IncrOp | EqSym
             
isDigit::Char->Bool
isDigit c = elem c ['0'..'9']

-- Takes a list of strings and converts it to a list of tokens
lexer::String->[Token]
lexer "" = []

-- Types
lexer xs | isPrefixOf "bool" xs = Type BoolType : lexer (drop 4 xs)
lexer xs | isPrefixOf "char" xs = Type CharType : lexer (drop 4 xs)
lexer xs | isPrefixOf "String" xs = Type StringType : lexer (drop 6 xs)
lexer('i':'n':'t':xs) = Type IntType:lexer xs

-- Keywords
lexer xs | isPrefixOf "for" xs = Keyword ForK : lexer (drop 3 xs)
lexer xs | isPrefixOf "return" xs = Keyword ReturnK : lexer (drop 6 xs)
lexer xs | isPrefixOf "if" xs = Keyword IfK : lexer (drop 2 xs)
lexer xs | isPrefixOf "then" xs = Keyword ThenK : lexer (drop 4 xs)
lexer xs | isPrefixOf "else" xs = Keyword ElseK : lexer (drop 4 xs)
lexer xs | isPrefixOf "for" xs = Keyword ForK : lexer (drop 3 xs)
lexer xs | isPrefixOf "while" xs = Keyword WhileK : lexer (drop 5 xs)

-- Constants
lexer (x:xs) | isDigit x = 
    let pref = takeWhile isDigit (x:xs)
        suff = dropWhile isDigit (x:xs)
        -- (pref, suff) = span isDigit (x:xs)
    in CSym (read pref) : lexer suff

-- Variables
lexer (x:xs) | isLower x =
    let (pref, suff) = span (isAlphaNum) (x:xs)
    in VSym  pref : lexer 

-- Operators
lexer ('+':'+':xs) = IncrOp : lexer xs
lexer ('+':xs) = BOp AddOp : lexer xs
-- Be careful with the ordering with the GT eq and just the GT
lexer ('>': '=':xs) = BOp GTE : lexer xs
lexer ('>':xs) = BOp Gt : lexer xs
lexer ('<':'=':xs) = BOp LTE : lexer xs
lexer ('<':xs) = BOp Lt : lexer xs

-- Punctuation
lexer ('{':xs) = LBra : lexer xs
lexer ('}':xs) = RBra : lexer xs
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (';':xs) = Semi : lexer xs