module Scanner.Token (Token(..), IndexedToken(..)) where 


data Token
     = IDENTIFIER String        -- id 
     | INTLITERAL Integer       -- num
     | LBRACKET                 -- {
     | RBRACKET                 -- }
     | LBRACE                   -- (
     | RBRACE                   -- )
     | LSQRBRACKET              -- [ (?) (-> arrays) 
     | RSQRBRACKET              -- ] (?) (-> arrays)
     | TILDE                    -- ~     (-> bitwise not)
     | EXCLMARK                 -- !
     | QUESMARK                 -- ? (?) (-> ternary operator)
     | DOT                      -- . 
     | SEMICOLON                -- ;
     | COLON                    -- :
     | COMMA                    -- ,
     | PLUS                     -- +
     | MINUS                    -- -
     | MUL                      -- *
     | DIV                      -- /
     | MOD                      -- %
     | LESS                     -- <
     | GREATER                  -- >
     | LOGICALOR                -- |
     | SHIFTLEFT                -- <<
     | SIGNEDSHIFTRIGHT         -- >>
     | UNSIGNEDSHIFTRIGHT       -- >>>
     
     | PUBLIC                   -- public 
     | PROTECTED                -- protected
     | PRIVATE                  -- private
     | STATIC                   -- static
     | ABSTRACT                 -- abstract

     | EXTENDS

     | CLASS                    -- class
     | THIS                     -- this
     | SUPER

     | ASSIGN                   -- =
     | NEW                      -- new

     | CHAR                     -- char
     | VOID                     -- void
     | BOOLEAN                  -- boolean
     | INT                      -- int
     | STRING                   -- String

     | IF                       -- if
     | WHILE                    -- while 
     | ELSE                     -- else
     | CASE                     -- case (maybe not to implement)
     | BREAK                    -- break

     | RETURN                   -- return 

     | EQUAL                    -- == 
     | NOTEQUAL                 -- !=
     | TIMESEQUAL               -- *=
     | DIVIDEEQUAL              -- /=
     | MODULOEQUAL              -- %=
     | PLUSEQUAL                -- +=
     | MINUSEQUAL               -- -=
     | ANDEQUAL                 -- &=
     | XOREQUAL                 -- ^=
     | OREQUAL                  -- |=
     | LESSEQUAL                -- <=
     | GREATEREQUAL             -- >=
     | SHIFTLEFTEQUAL           -- <<=
     | SIGNEDSHIFTRIGHTEQUAL    -- >>=
     | UNSIGNEDSHIFTRIGHTEQUAL  -- >>>=

     | INSTANCEOF               -- instanceof(Type)

     | INCREMENT                -- ++
     | DECREMENT                -- --

     | AND                      -- &&
     | OR                       -- ||
     | XOR                      -- ^

     | BOOLLITERAL Bool         -- true | false
     | CHARLITERAL Char         -- '..'
     | STRINGLITERAL String     -- ".."
     | JNULL                    -- null

     | NEWLINE                  -- temporally holds information about line-numbers
     deriving (Show, Eq)


-- represents Token with line-number in source-file
data IndexedToken = IndexedToken
 { index :: Integer, token :: Token } deriving (Show, Eq)