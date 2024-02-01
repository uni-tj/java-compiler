module Parser.Combinators where

import Scanner.Token
import Data.Bifoldable (bifoldl1)

type Parser toks a = [toks] -> [(a, [toks])]

failure :: Parser a b                       -- Parser der leeren Sprache
failure _ = []

succeed :: a -> Parser tok a                -- Parser der Sprache des 
succeed value toks = [(value, toks)]        -- leeren Worts (\epsilon)

-- conditional recognition
satisfy :: (tok -> Bool) -> Parser tok tok 
satisfy _ [] = []
satisfy cond (tkn : tkns) | cond tkn  = succeed tkn tkns
                          | otherwise = failure tkns

{-
-- AIO recognition a singular token with error handling and position tracking
recon :: Token -> ParserWithError PositionedToken PositionedToken
recon tkn [] = [Left ("unexpected end of Input, expected: " ++ show tkn)]
recon tkn (tok : toks)  | equivalent tkn (token tok) = [Right (tok, toks)]
                        | otherwise = [Left ("unexpected token: " ++ show tok ++ ", expected: " ++ show tkn 
                                            ++ " at " ++ show (position tok))]
            where
                equivalent (IDENTIFIER _) (IDENTIFIER _) = True
                equivalent (INTLITERAL _) (INTLITERAL _) = True
                equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
                equivalent (CHARLITERAL _) (CHARLITERAL _) = True
                equivalent tkn1 tkn2 = tkn1 == tkn2
-}
-- functions to recognize tokens with error handling


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

-- parse zero or n-many times the thing, p parses, and concat to list
many :: Parser tok a -> Parser tok [a]
many parser =     (parser <<< (: []))
              ||| ((parser +.+ many parser) <<< uncurry (:))
              ||| succeed []



-- error handling: 

type ParseError = String 

type EParser tok a = [tok] -> [(Either ParseError a, [tok])]

failureE :: EParser a b
failureE _ = [(Left "unexpected end of Input", [])]

satisfyE :: Show tok => (tok -> Bool) -> EParser tok tok 
satisfyE _ [] = [(Left "unexpected end of Input", [])]
satisfyE cond (tkn : tkns) | cond tkn  = [(Right tkn, tkns)]
                           | otherwise = [(Left ("unexpected token: " ++ show tkn), tkns)]

succeedE :: a -> EParser tok a
succeedE value toks = [(Right value, toks)]

manyE :: EParser tok a -> EParser tok [a] 
manyE parser =    (parser >>> (: []))
              <|> ((parser +++ manyE parser) >>> uncurry (:))
              <|> succeedE []

recogn :: Eq tok => Show tok => tok -> EParser tok tok
recogn tkn = satisfyE (== tkn)


item :: Token -> EParser PositionedToken PositionedToken
item tkn = satisfyE (equivalent tkn . token)  where 
        equivalent (IDENTIFIER _) (IDENTIFIER _) = True
        equivalent (INTLITERAL _) (INTLITERAL _) = True
        equivalent (BOOLLITERAL _) (BOOLLITERAL _) = True
        equivalent (CHARLITERAL _) (CHARLITERAL _) = True
        equivalent tkn1 tkn2 = tkn1 == tkn2

-- The choice combinator
(<|>) :: EParser tok a -> EParser tok a -> EParser tok a
(p1 <|> p2) toks = let res1 = p1 toks
                       res2 = p2 toks
                   in case (res1, res2) of
                        ([(Right v1, rest1)], [(Left _, _)]) -> [(Right v1, rest1)]
                        ([(Left _, _)], [(Right v2, rest2)]) -> [(Right v2, rest2)]
                        ([(Right v1, rest1)], [(Right v2, rest2)]) -> [(Right v1, rest1), (Right v2, rest2)]
                        ([(Left err1, _)],[(Left err2, _)]) -> [(Left (err1 ++ " or " ++ err2),[])]
                        _ -> res1 ++ res2

-- The bind combinator
(>>>) :: EParser tok a -> (a -> b) -> EParser tok b
(p >>> f) toks = [ case v of
                    Left err -> (Left err, rest)
                    Right val -> (Right (f val), rest)
                 | (v, rest) <- p toks]

-- The sequence combinator
(+++) :: EParser tok a -> EParser tok b -> EParser tok (a, b)
(p1 +++ p2) toks =  case p1 toks of 
            [(Left err, _)] -> [(Left err, [])]
            [(Right v1, rest1)] -> case p2 rest1 of
                    [(Left err, _)] -> [(Left err, [])]
                    [(Right v2, rest2)] -> [(Right (v1, v2), rest2)]
                    _ -> [(Left "unexpected error1", [])]
            _ -> [(Left "unexpected error2", [])]

infixr +++, <|>