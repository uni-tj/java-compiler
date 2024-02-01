module Scanner.Token (Token(..), PositionedToken(..)) where

import Types.AST -- to access Position type


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
     
     | OVERRIDE                 -- @Override

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
     | CASE                     -- case     (not to implement)
     | BREAK                    -- break    (not to implement)
     | CONTINUE                 -- continue (not to implement)
     | FOR                      -- for

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

     | INCREMENT                -- ++ (not to Implement)
     | DECREMENT                -- -- (not to implement)

     | AND                      -- &&
     | OR                       -- ||
     | XOR                      -- ^ (not to implement)

     | BOOLLITERAL Bool         -- true | false
     | CHARLITERAL Char         -- '..'
     | STRINGLITERAL String     -- ".."
     | JNULL                    -- null

     | NEWLINE                  -- temporally holds information about positions
     | SPACE 
     | WRONGTOKEN String Int    -- error token with message and original size
     deriving (Show, Eq)


-- represents Token with line-number in source-file
data PositionedToken = PositionedToken
 { position :: Position, token :: Token }

instance Show PositionedToken where
     show (PositionedToken _ t) = show t
