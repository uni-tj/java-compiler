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
lexer ('\'':ch: '\'':cs) = CHARLITERAL ch : lexer cs
lexer ('\'':ch: cs) = WRONGTOKEN ("unclosed char: " ++ [ch]) 1 : lexer cs
lexer ['\''] = [WRONGTOKEN "unclosed char" 0]
lexer ('/' : '/' : xs) = lexInLineComment xs
lexer ('/' : '*' : xs) = lexMulLineComment xs
lexer (c:cs)
      | c == '\n' = NEWLINE : lexer cs    -- to remember the line number
      | c == ' ' = SPACE : lexer cs
      | c == '\t' = SPACE : lexer cs      -- mapping tabs to space characters
      | isSpace c = lexer cs              -- skip ' ', \t, \n, \r, \f, \v
      | isAlpha c = lexVar (c:cs)         -- lex Bool, Char, id...
      | isDigit c = lexNum (c:cs)         -- lex a number 
      | c == '"' = lexStr cs              -- lex a String
lexer ('!':'=':cs) = NOTEQUAL : lexer cs
lexer ('<':'=':cs) = LESSEQUAL : lexer cs
lexer ('>':'=':cs) = GREATEREQUAL : lexer cs

lexer ('=':'=':cs) = EQUAL : lexer cs
lexer ('{':cs) = LBRACKET : lexer cs
lexer ('}':cs) = RBRACKET : lexer cs
lexer ('(':cs) = LBRACE : lexer cs
lexer (')':cs) = RBRACE : lexer cs
lexer ('[':cs) = LSQRBRACKET : lexer cs
lexer (']':cs) = RSQRBRACKET : lexer cs
lexer ('!':cs) = EXCLMARK : lexer cs
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
lexer ('|' : '|' : cs) = OR : lexer cs
lexer ('=':cs) = ASSIGN : lexer cs

lexer (someChar : cs) = WRONGTOKEN ("token not supported :" ++ [someChar]) 1 : lexer cs

{-- <unsupported tokens>:
lexer ('+':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('-':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('&':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('|':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('/':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('*':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('%':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('^':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('|':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('^':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('&':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('?':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('~':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('<':'<':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('>':'>':'=':cs) =WRONGTOKEN "not supported" 2 : lexer cs
lexer ('>':'>':'>':'=':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('<':'<':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('>':'>':'>':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer ('>':'>':cs) = WRONGTOKEN "not supported" 2 : lexer cs
lexer (':' : cs) = WRONGTOKEN "not supported" 1 : lexer cs
lexer ('+':'+':cs) = INCREMENT : lexer cs
lexer ('-':'-':cs) = DECREMENT : lexer cs
-- </unsupported tokens> -}


lexNum :: String -> [Token]
lexNum cs = INTLITERAL (read num) : lexer rest
      where (num,rest) = span isDigit cs

-- this will lead to an error after while scanning
lexStr :: String -> [Token]
lexStr cs = STRINGLITERAL (read str) : lexer rest
       where (str,rest) = span ('"' /=) cs


lexVar :: String -> [Token]
lexVar cs =
   case span isAlphaNum cs of
      ("true",rest) -> BOOLLITERAL True : lexer rest
      ("false",rest) -> BOOLLITERAL False : lexer rest
      ("public",rest) -> PUBLIC : lexer rest
      ("protected",rest) -> PROTECTED : lexer rest
      ("private",rest) -> PRIVATE : lexer rest
      ("static",rest) -> STATIC : lexer rest
      ("class",rest) -> CLASS : lexer rest
      ("this",rest) -> THIS : lexer rest
      ("super", rest) -> SUPER : lexer rest
      ("new",rest) -> NEW : lexer rest
      ("String",rest) -> STRING : lexer rest
      ("char",rest) -> CHAR : lexer rest
      ("void",rest) -> VOID : lexer rest
      ("boolean",rest) -> BOOLEAN : lexer rest
      ("int",rest) -> INT : lexer rest
      ("if",rest) -> IF : lexer rest
      ("while",rest) -> WHILE : lexer rest
      ("else",rest) -> ELSE : lexer rest
      ("return",rest) -> RETURN : lexer rest
      ("null",rest) -> JNULL : lexer rest
      ("extends", rest) -> EXTENDS : lexer rest
      (var,rest)   -> IDENTIFIER var : lexer rest

-- unsupported tokens
   -- ("for", rest) -> FOR : lexer rest
   -- ("case",rest) -> CASE : lexer rest
   -- ("break",rest) -> BREAK : lexer rest
   -- ("instanceof",rest) -> INSTANCEOF : lexer rest
   -- ("abstract",rest) -> ABSTRACT : lexer rest

-- comments -- 

lexMulLineComment :: String -> [Token]
lexMulLineComment ('*' : '/' : xs) = lexer xs
lexMulLineComment ('\n' : xs) = NEWLINE : lexMulLineComment xs
lexMulLineComment (_ : xs) = lexMulLineComment xs
lexMulLineComment [] = error "unclosed multi line comment"

lexInLineComment :: String -> [Token]
lexInLineComment [] = []
lexInLineComment ('\n' : xs) = NEWLINE : lexer xs
lexInLineComment (_ : xs) = lexInLineComment xs


-- # adding line information to tokens # --

-- filtering a Token list to not contain NEWLINE's anymore
filterTokens :: [Token] -> [Token]
filterTokens = filter (/= NEWLINE)

tokenLength :: Token -> Integer
tokenLength (WRONGTOKEN _ len) = toInteger len
tokenLength (IDENTIFIER str) = toInteger (length str)
tokenLength (INTLITERAL x) = toInteger (length (show x))
tokenLength PUBLIC = 6 
tokenLength PROTECTED = 9 
tokenLength PRIVATE = 7
tokenLength STATIC = 6 
tokenLength EXTENDS = 8
tokenLength CLASS = 5
tokenLength THIS = 4
tokenLength SUPER = 5
tokenLength NEW = 3
tokenLength CHAR = 4
tokenLength VOID = 4
tokenLength BOOLEAN = 7
tokenLength INT = 3
tokenLength IF = 2
tokenLength WHILE = 5
tokenLength ELSE = 4
tokenLength FOR = 3
tokenLength RETURN = 6
tokenLength EQUAL = 2
tokenLength NOTEQUAL = 2
tokenLength AND = 2
tokenLength OR = 2
tokenLength (BOOLLITERAL tr) = if tr then 4 else 5
tokenLength (CHARLITERAL ch) | isSpace ch = 2 --in case of escape characters '\n', '\t', ...
                             | otherwise = 1  --standard character
tokenLength JNULL = 4 
tokenLength _ = 1


-- indexing Tokens based on occurences of NEWLINE token (will be removed)
indexTokens :: [Token] -> [PositionedToken]
indexTokens [] = []
indexTokens list = indexTokensRec 1 1 list where
    indexTokensRec _ _ [] = []
    indexTokensRec _ vpos (NEWLINE : tkns) = indexTokensRec 1 (vpos + 1) tkns
    indexTokensRec hpos vpos (SPACE : tkns) = indexTokensRec (hpos + 1) vpos tkns
    indexTokensRec hpos vpos (tkn : tkns) = 
      PositionedToken { position = Position {start = (hpos, vpos), end = (hpos + (tokenLength tkn), vpos)},
         token = tkn } : indexTokensRec (hpos + (tokenLength tkn)) vpos tkns

-- should be redundant, because indexTokens already filters all NEWLINE's
filterIndexedTokens :: [PositionedToken] -> [PositionedToken]
filterIndexedTokens =
   filter (\posTkn -> token posTkn /= NEWLINE && token posTkn /= SPACE)

-- final validation before carrying on with parsing

isValidInt :: Integer -> Bool
isValidInt num = (num <= 2147483647) && (num >= -2147483648)

validateTokens :: [Token] -> [Token]
validateTokens [] = []
validateTokens ((INTLITERAL x) : tkns) = if isValidInt x then INTLITERAL x : validateTokens tkns else
   error (show x ++ " is out of range for int")
validateTokens ((STRINGLITERAL str) : _) =
   error ("strings not supported yet :" ++ str)
validateTokens ((WRONGTOKEN msg _) : _) = error msg
validateTokens (tkn : tkns) = tkn : validateTokens tkns
