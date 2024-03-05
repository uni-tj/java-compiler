module Scanner.Lexer (scanner) where

import Scanner.Token
import Types.AST -- for Position type
import Data.Char
import Error.PrintError

scanner :: String -> [PositionedToken]
scanner fl = lexWithIndex (lines fl) fl

lexWithIndex :: File -> String -> [PositionedToken]
lexWithIndex fl =  validateTokens fl . indexTokens . lexer

lexer :: String -> [Token]
lexer [] = []
lexer ('\'':ch: '\'':cs) = CHARLITERAL ch : lexer cs
lexer ('\'':ch: cs) = WRONGTOKEN ("unclosed character literal: " ++ [ch]) 1 : lexer cs
lexer ['\''] = [WRONGTOKEN "unclosed character literal" 0]
lexer ('/' : '/' : xs) = lexInLineComment xs
lexer ('/' : '*' : xs) = SPACE : SPACE : lexMulLineComment xs -- (spaces for positioning)
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
lexer ('@':'O':'v':'e':'r':'r':'i':'d':'e': cs) = OVERRIDE : lexer cs

lexer (someChar : cs) = WRONGTOKEN ("token not supported :" ++ [someChar]) 1 : lexer cs -- to make pattern match exhaustive


lexNum :: String -> [Token]
lexNum cs = INTLITERAL (read num) : lexer rest
      where (num,rest) = span isDigit cs

-- this will lead to an error later, while scanning
lexStr :: String -> [Token]
lexStr cs = STRINGLITERAL str : lexer rest
       where (str,rest) = span (/= '"') cs


lexVar :: String -> [Token]
lexVar cs =
   case span isAlphaNum cs of
      ("true",rest)        -> BOOLLITERAL True : lexer rest
      ("false",rest)       -> BOOLLITERAL False : lexer rest
      ("public",rest)      -> PUBLIC : lexer rest
      ("protected",rest)   -> PROTECTED : lexer rest
      ("private",rest)     -> PRIVATE : lexer rest
      ("static",rest)      -> STATIC : lexer rest
      ("class",rest)       -> CLASS : lexer rest
      ("this",rest)        -> THIS : lexer rest
      ("super", rest)      -> SUPER : lexer rest
      ("new",rest)         -> NEW : lexer rest
      ("String",rest)      -> STRING : lexer rest
      ("char",rest)        -> CHAR : lexer rest
      ("void",rest)        -> VOID : lexer rest
      ("boolean",rest)     -> BOOLEAN : lexer rest
      ("int",rest)         -> INT : lexer rest
      ("if",rest)          -> IF : lexer rest
      ("while",rest)       -> WHILE : lexer rest
      ("else",rest)        -> ELSE : lexer rest
      ("return",rest)      -> RETURN : lexer rest
      ("null",rest)        -> JNULL : lexer rest
      ("extends", rest)    -> EXTENDS : lexer rest
      (var,rest)           -> IDENTIFIER var : lexer rest


-- comments -- 
lexMulLineComment :: String -> [Token]
lexMulLineComment ('*' : '/' : xs) = SPACE : SPACE : lexer xs -- (space for positioning)
lexMulLineComment ('\n' : xs) = NEWLINE : lexMulLineComment xs -- (newline for positioning)
lexMulLineComment (_ : xs) = SPACE : lexMulLineComment xs -- (space for positioning)
lexMulLineComment [] = error "unclosed multi line comment" -- scanner error when a comment is not closed

lexInLineComment :: String -> [Token]
lexInLineComment [] = []
lexInLineComment ('\n' : xs) = NEWLINE : lexer xs
lexInLineComment (_ : xs) = lexInLineComment xs


-- # adding position information to tokens # --


tokenLength :: Token -> Int
tokenLength (CHARLITERAL ch) | isSpace ch = 4 --in case of escape characters '\n', '\t', ...
                             | otherwise = 3  --standard character
tokenLength tkn = case tkn of
         (WRONGTOKEN _ len)   -> len
         (IDENTIFIER str)     -> (length str) + 2
         (INTLITERAL x)       -> (length (show x))
         PUBLIC               -> 6
         PROTECTED            -> 9
         PRIVATE              -> 7
         STATIC               -> 6
         EXTENDS              -> 8
         CLASS                -> 5
         THIS                 -> 4
         SUPER                -> 5
         NEW                  -> 3
         CHAR                 -> 4
         VOID                 -> 4
         BOOLEAN              -> 7
         INT                  -> 3
         IF                   -> 2
         WHILE                -> 5
         ELSE                 -> 4
         RETURN               -> 6
         EQUAL                -> 2
         NOTEQUAL             -> 2
         AND                  -> 2
         OR                   -> 2
         (BOOLLITERAL tr)     -> if tr then 4 else 5
         JNULL                -> 4
         OVERRIDE             -> 8
         _                    -> 1 -- make pattern match exhaustive


-- indexing Tokens based on occurences of NEWLINE token (will be removed)
indexTokens :: [Token] -> [PositionedToken]
indexTokens [] = []
indexTokens list = indexTokensRec 1 1 list where
    indexTokensRec _ _ [] = []
    indexTokensRec _ vpos (NEWLINE : tkns) = indexTokensRec 1 (vpos + 1) tkns
    indexTokensRec hpos vpos (SPACE : tkns) = indexTokensRec (hpos + 1) vpos tkns
    indexTokensRec hpos vpos (tkn : tkns) =
      PositionedToken { 
                        position = Position {
                                       start = (vpos, hpos), 
                                       end = (vpos, hpos + tokenLength tkn)
                                    },
                        token = tkn 
                     } : indexTokensRec (hpos + tokenLength tkn) vpos tkns



-- validation of tokens after indexing
isValidInt :: Integer -> Bool
isValidInt num = (num <= 2147483647) && (num >= -2147483648)

validateTokens :: File -> [PositionedToken] -> [PositionedToken]
validateTokens _  []  = []
validateTokens fl ((PositionedToken { position = pos, token = (INTLITERAL x)}) : tkns) = 
   if isValidInt x then (PositionedToken { position = pos, token = INTLITERAL x}) : validateTokens fl tkns 
      else error (printError fl (show x ++ " is too large for an integer literal") pos) -- throw an error for too |large| int-literals
validateTokens fl ((PositionedToken { position = pos, token = (STRINGLITERAL str)}) : _) =
   error (printError fl ("strings not supported yet :" ++ str) pos) -- throw an error, when strings occur
validateTokens fl (PositionedToken { position = pos, token = (WRONGTOKEN msg _) } : _) = 
   error (printError fl msg pos)
validateTokens _ (PositionedToken { token = NEWLINE} : _) = 
   error "internal error: newline-token was not removed"
validateTokens _ (PositionedToken { token = SPACE} : _) = 
   error "internal error: space-token was not removed"
validateTokens fl (tkn : tkns) = tkn : validateTokens fl tkns
