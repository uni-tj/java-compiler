module Scanner.Lexer (lexWithIndex, lexWithoutIndex) where 

import Scanner.Token
import Types.AST -- for Position type
import Data.Char


lexWithIndex :: String -> [PositionedToken]
lexWithIndex = filterIndexedTokens . indexTokens . validateTokens . lexer 

lexWithoutIndex :: String -> [Token] 
lexWithoutIndex = filterTokens . validateTokens . lexer


lexer :: String -> [Token]
lexer [] = []
lexer ('\'':ch: '\'':cs) = (CHARLITERAL ch) : (lexer cs)
lexer ('/' : '/' : xs) = lexInLineComment xs
lexer ('/' : '*' : xs) = lexMulLineComment xs
lexer (c:cs)
      | (c == '\n') = NEWLINE : lexer cs -- to remember the line number
      | isSpace c = lexer cs        -- skip ' ', \t, \n, \r, \f, \v
      | isAlpha c = lexVar (c:cs)   -- lex Bool, Char, id...
      | isDigit c = lexNum (c:cs)   -- lex a number 
   -- | (c == '"') = lexStr (cs)    -- lex a String

lexer ('!':'=':cs) = NOTEQUAL : lexer cs
--lexer ('+':'=':cs) = PLUSEQUAL : lexer cs
--lexer ('-':'=':cs) = MINUSEQUAL : lexer cs
--lexer ('&':'=':cs) = ANDEQUAL : lexer cs
--lexer ('|':'=':cs) = OREQUAL : lexer cs
lexer ('<':'=':cs) = LESSEQUAL : lexer cs
lexer ('>':'=':cs) = GREATEREQUAL : lexer cs
lexer ('+':'+':cs) = INCREMENT : lexer cs
lexer ('-':'-':cs) = DECREMENT : lexer cs
--lexer ('/':'=':cs) = DIVIDEEQUAL : lexer cs
--lexer ('*':'=':cs) = TIMESEQUAL : lexer cs
--lexer ('%':'=':cs) = MODULOEQUAL : lexer cs
--lexer ('^':'=':cs) = XOREQUAL : lexer cs
lexer ('=':'=':cs) = EQUAL : lexer cs
--lexer ('<':'<':'=':cs) = SHIFTLEFTEQUAL : lexer cs
--lexer ('>':'>':'=':cs) = SIGNEDSHIFTRIGHTEQUAL : lexer cs
--lexer ('>':'>':'>':'=':cs) = UNSIGNEDSHIFTRIGHTEQUAL : lexer cs

--lexer ('<':'<':cs) = SHIFTLEFT : lexer cs
--lexer ('>':'>':'>':cs) = UNSIGNEDSHIFTRIGHT : lexer cs
--lexer ('>':'>':cs) = SIGNEDSHIFTRIGHT : lexer cs

lexer ('{':cs) = LBRACKET : lexer cs
lexer ('}':cs) = RBRACKET : lexer cs
lexer ('(':cs) = LBRACE : lexer cs
lexer (')':cs) = RBRACE : lexer cs
lexer ('[':cs) = LSQRBRACKET : lexer cs 
lexer (']':cs) = RSQRBRACKET : lexer cs
--lexer ('~':cs) = TILDE : lexer cs
lexer ('!':cs) = EXCLMARK : lexer cs
--lexer ('?':cs) = QUESMARK : lexer cs
lexer ('.':cs) = DOT : lexer cs
lexer (';':cs) = SEMICOLON : lexer cs
lexer (':':cs) = COLON : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer ('-':cs) = MINUS : lexer cs
lexer ('*':cs) = MUL : lexer cs
lexer ('/':cs) = DIV : lexer cs
lexer ('%':cs) = MOD : lexer cs
lexer ('<':cs) = LESS : lexer cs
lexer ('>':cs) = GREATER : lexer cs
lexer ('&':'&':cs) = AND : lexer cs
--lexer ('&':cs) = AND : lexer cs
lexer ('|' : '|' : cs) = OR : lexer cs
--lexer ('|':cs) = OR : lexer cs
--lexer ('^':cs) = XOR : lexer cs
lexer ('=':cs) = ASSIGN : lexer cs




lexNum :: String -> [Token]
lexNum cs = INTLITERAL (read num) : lexer rest
      where (num,rest) = span isDigit cs

{-
lexStr :: String -> [Token]
lexStr cs = STRINGLITERAL (read str) : lexer rest
       where (str,rest) = span ((/=) '"') cs
-}

lexVar :: String -> [Token]
lexVar cs =
   case span isAlphaNum cs of

      ("true",rest) -> BOOLLITERAL True : lexer rest
      ("false",rest) -> BOOLLITERAL False : lexer rest

      ("public",rest) -> PUBLIC : lexer rest
      ("protected",rest) -> PROTECTED : lexer rest
      ("private",rest) -> PRIVATE : lexer rest
      ("static",rest) -> STATIC : lexer rest
   -- ("abstract",rest) -> ABSTRACT : lexer rest

      ("class",rest) -> CLASS : lexer rest
      ("this",rest) -> THIS : lexer rest
      ("super", rest) -> SUPER : lexer rest

      ("new",rest) -> NEW : lexer rest

      ("String",rest) -> STRING : lexer rest -- ! fix capitalisation
      ("char",rest) -> CHAR : lexer rest
      ("void",rest) -> VOID : lexer rest
      ("boolean",rest) -> BOOLEAN : lexer rest
      ("int",rest) -> INT : lexer rest

      ("if",rest) -> IF : lexer rest
      ("while",rest) -> WHILE : lexer rest
      ("else",rest) -> ELSE : lexer rest
   -- ("case",rest) -> CASE : lexer rest
   -- ("break",rest) -> BREAK : lexer rest

      ("return",rest) -> RETURN : lexer rest

   -- ("instanceof",rest) -> INSTANCEOF : lexer rest

      ("null",rest) -> JNULL : lexer rest

      ("extends", rest) -> EXTENDS : lexer rest

      (var,rest)   -> IDENTIFIER var : lexer rest

-- comments -- 

lexMulLineComment :: String -> [Token]
lexMulLineComment ('*' : '/' : xs) = lexer xs
lexMulLineComment ('\n' : xs) = NEWLINE : lexMulLineComment xs
lexMulLineComment (x : xs) = lexMulLineComment xs

lexInLineComment :: String -> [Token]
lexInLineComment ('\n' : xs) = NEWLINE : lexer xs
lexInLineComment (x : xs) = lexInLineComment xs


-- adding line information to tokens --


-- filtering a Token list to not contain NEWLINE's anymore
filterTokens :: [Token] -> [Token]
filterTokens = filter (/= NEWLINE)

--placeholder for position
dummyPos :: Integer 
dummyPos = 0

-- indexing Tokens based on occurences of NEWLINE token (will be removed)
indexTokens :: [Token] -> [PositionedToken]
indexTokens [] = [] 
indexTokens list = indexTokensRec 0 list where 
    indexTokensRec _ [] = [] 
    indexTokensRec idx (NEWLINE : tkns) = indexTokensRec (idx + 1) tkns
    indexTokensRec idx (tkn : tkns) = 
      PositionedToken { position = Position {start = (idx, dummyPos), end = (idx, dummyPos)},
         token = tkn } : indexTokensRec idx tkns

-- should be redundant, because indexTokens already filters all NEWLINE's
filterIndexedTokens :: [PositionedToken] -> [PositionedToken]
filterIndexedTokens = filter (\posTkn -> token posTkn /= NEWLINE)

-- validation 

isValidInt :: Integer -> Bool
isValidInt num = (num <= 2147483647) && (num >= -2147483648)

validateTokens :: [Token] -> [Token]
validateTokens [] = []
validateTokens ((INTLITERAL x) : tkns) = if isValidInt x then INTLITERAL x : validateTokens tkns else 
   error (show x ++ " is out of range for int")

validateTokens (tkn : tkns) = tkn : validateTokens tkns
