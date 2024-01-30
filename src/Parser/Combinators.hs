module Parser.Combinators where

import Scanner.Token

type Parser toks a = [toks] -> [(a, [toks])]

-- for now an error is just a String
type ParseError = String

type ParserWithError tok a = [tok] -> Either ParseError (a, [tok]) 

failure :: Parser a b                       -- Parser der leeren Sprache
failure _ = []

succeed :: a -> Parser tok a                -- Parser der Sprache des 
succeed value toks = [(value, toks)]        -- leeren Worts (\epsilon)

-- conditional recognition
satisfy :: (tok -> Bool) -> Parser tok tok 
satisfy _ [] = []
satisfy cond (tkn : tkns) | cond tkn  = succeed tkn tkns
                          | otherwise = failure tkns



-- AIO recognition a singular token with error handling and position tracking
recon :: PositionedToken -> ParserWithError PositionedToken PositionedToken
recon tkn [] = Left ("unexpected end of Input, expected: " ++ show (token tkn))
recon tkn (tok : toks)  | equivalent (token tkn) (token tok) = Right (tok, toks)
                        | otherwise = Left ("unexpected token: " ++ show tok ++ ", expected: " ++ show (token tkn) 
                                            ++ "at " ++ show (position tkn))
            where
                equivalent (IDENTIFIER _) (IDENTIFIER _) = True
                equivalent (INTLITERAL _) (INTLITERAL _) = True
                equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
                equivalent (CHARLITERAL _) (CHARLITERAL _) = True
                equivalent tkn1 tkn2 = tkn1 == tkn2

-- functions to recognize tokens with error handling
satisfyE :: Show tok =>  (tok -> Bool) -> ParserWithError tok tok
satisfyE _ [] = Left "unexpected end of Input"
satisfyE cond (tkn : tkns)  | cond tkn = Right (tkn, tkns)
                            | otherwise = Left ("unexpected token: " ++ show tkn)

lexemE :: (Eq tok, Show tok) => tok -> ParserWithError tok tok
lexemE tkn = satisfyE (== tkn)

-- recognition of a lexem (terminal)
lexem :: Token -> Parser Token Token
lexem tkn = satisfy (== tkn)

-- recognition of Lexems with contructor parameters
lexemParam :: Token -> Parser Token Token
lexemParam tok = satisfy (equivalent tok) where
            equivalent (IDENTIFIER _) (IDENTIFIER _) = True
            equivalent (INTLITERAL _) (INTLITERAL _) = True
            equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
            equivalent (CHARLITERAL _) (CHARLITERAL _) = True
            equivalent tkn1 tkn2 = tkn1 == tkn2


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

-- combinators, semanitally identicalwith but with error handling support
(+++) :: ParserWithError tok a -> ParserWithError tok b -> ParserWithError tok (a,b)
(p1 +++ p2) toks = case p1 toks of
    Left err -> Left err
    Right (v1, rest1) -> case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) -> Right ((v1, v2), rest2)

(<||>) :: ParserWithError tok a -> ParserWithError tok a -> ParserWithError tok a
(p1 <||> p2) toks = case p1 toks of
    Left err -> case p2 toks of
        Left err2 -> Left (err ++ " and " ++ err2) -- might wanna changes this to "or" in the future
        Right (v2, rest2) -> Right (v2, rest2)
    Right (v1, rest1) -> Right (v1, rest1)

--alternative combinator to use with error handling
(<|>) :: ParserWithError tok a -> ParserWithError tok a -> ParserWithError tok a
(p1 <|> p2) toks = case p1 toks of
    Left err -> case p2 toks of
        Left err2 -> Left err2 -- might wanna changes this to "or" in the future
        Right (v2, rest2) -> Left "meta error: base case error not correct"
    Right (v1, rest1) -> Right (v1, rest1)


(<$$>) :: ParserWithError tok a -> (a -> b) -> ParserWithError tok b
(p <$$> f) toks = case p toks of
    Left err -> Left err
    Right (v, rest) -> Right (f v, rest)
infixr +++, <||>


-- parse zero or n-many times the thing, p parses, and concat to list
many :: Parser tok a -> Parser tok [a]
many parser =     (parser <<< (: []))
              ||| ((parser +.+ many parser) <<< uncurry (:))
              ||| succeed []