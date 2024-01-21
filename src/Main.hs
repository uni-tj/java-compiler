module Main where 

import Scanner.Lexer ( lexWithoutIndex )
import Parser.Parser (parseExpr)
import Types.AST

test :: String
test = "2 + 4 * 5"


main :: IO()
main = do
    print(parseExpr (lexWithoutIndex test))