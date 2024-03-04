module Parser.Combinators where

import Scanner.Token

type Parser toks a = [toks] -> [(a, [toks])]

-- Parser of the empty Language
failure :: Parser a b
failure _ = []

-- Parsing the empty word
succeed :: a -> Parser tok a
succeed value toks = [(value, toks)]

-- conditional recognition
satisfy :: (tok -> Bool) -> Parser tok tok 
satisfy _ [] = []
satisfy cond (tkn : tkns) | cond tkn  = succeed tkn tkns
                          | otherwise = failure tkns

-- recognition of Lexems (ignoring possible constructor parameters)
lexem :: Token -> Parser Token Token
lexem tok = satisfy (equivalent tok) where
            equivalent (IDENTIFIER _) (IDENTIFIER _) = True
            equivalent (INTLITERAL _) (INTLITERAL _) = True
            equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
            equivalent (CHARLITERAL _) (CHARLITERAL _) = True
            equivalent tkn1 tkn2 = tkn1 == tkn2


--the same functions, just for recognition of tokens, wrapped with their Position
posLexem :: Token -> Parser PositionedToken PositionedToken
posLexem tkn = satisfy (equivalent tkn . token) where
            equivalent (IDENTIFIER _) (IDENTIFIER _) = True
            equivalent (INTLITERAL _) (INTLITERAL _) = True
            equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
            equivalent (CHARLITERAL _) (CHARLITERAL _) = True
            equivalent tok1 tok2 = tok1 == tok2


-- sequential recognition
(+.+) :: Parser tok a -> Parser tok b -> Parser tok (a,b)
(p1 +.+ p2) toks = [((v1, v2), rest2) | (v1, rest1) <- p1 toks,
                                        (v2, rest2) <- p2 rest1]
-- alternative
(|||) :: Parser tok a -> Parser tok a -> Parser tok a
(p1 ||| p2) toks = p1 toks ++ p2 toks

(<<<) :: Parser tok a -> (a -> b) -> Parser tok b
(p <<< f) toks = [ (f v, rest) | (v, rest) <- p toks]

infixr +.+, |||

-- parse zero or n-many times the thing, p parses, and concat to list
many :: Parser tok a -> Parser tok [a]
many parser =     (parser <<< (: []))
              ||| ((parser +.+ many parser) <<< uncurry (:))
              ||| succeed []
