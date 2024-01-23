module Parser.Combinators where

import Scanner.Token

type Parser toks a = [toks] -> [(a, [toks])]


failure :: Parser a b                       -- Parser der leeren Sprache
failure = \_ -> []


succeed :: a -> Parser tok a                -- Parser der Sprache des 
succeed value toks = [(value, toks)]        -- leeren Worts (\epsilon)


-- conditional recognition
satisfy :: (tok -> Bool) -> Parser tok tok
satisfy _ [] = []
satisfy cond (tok : toks) | cond tok  = succeed tok toks
                          | otherwise = failure toks

-- recognition of a lexem (terminal)
lexem :: Eq tok => tok -> Parser tok tok
lexem tok = satisfy (== tok)

-- recognition of Lexems with contructor parameters
lexemParam :: Token -> Parser Token Token
lexemParam tok = satisfy (equivalent tok) where
            equivalent (IDENTIFIER _) (IDENTIFIER _) = True
            equivalent (INTLITERAL _) (INTLITERAL _) = True
            equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
            equivalent (CHARLITERAL _) (CHARLITERAL _) = True
            equivalent tok1 tok2 = tok1 == tok2

--the same functions, just for recognition of tokens, wrapped with their Position
posLexem :: Token -> Parser PositionedToken PositionedToken
posLexem tkn = satisfy (\posTkn -> token posTkn == tkn)

posLexemParam :: Token -> Parser PositionedToken PositionedToken
posLexemParam tkn = satisfy (equivalent tkn . token) where
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

-- parse one or n-many times the thing, p parses, and concat to list

many :: Parser tok a -> Parser tok [a]
many parser = (parser <<< (: [])) ||| ((parser +.+ many parser) <<< uncurry (:)) ||| succeed []

{-
--error handling
parseWithError :: Parser tok a -> String -> Parser tok a
parseWithError parser msg = \toks -> 
    let 
        result = parser toks 
    in 
        subParser result 

    where 
        subParser ((parseOutput, []) : rest) = (parseOutput, []) : rest
        subParser ((parseOutput, restTokens) : rest) = error msg

-}