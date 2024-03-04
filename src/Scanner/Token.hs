module Scanner.Token (Token(..), PositionedToken(..)) where

import Types.AST -- to access Position type

data Token
     = IDENTIFIER String        -- id 
     | INTLITERAL Integer       -- num
     | LBRACKET                 -- {
     | RBRACKET                 -- }
     | LBRACE                   -- (
     | RBRACE                   -- )
     | LSQRBRACKET              -- [ 
     | RSQRBRACKET              -- ]
     | EXCLMARK                 -- !
     | DOT                      -- . 
     | SEMICOLON                -- ;
     | COMMA                    -- ,
     | PLUS                     -- +
     | MINUS                    -- -
     | MUL                      -- *
     | DIV                      -- /
     | MOD                      -- %
     | LESS                     -- <
     | GREATER                  -- >
     
     | OVERRIDE                 -- @Override

     | PUBLIC                   -- public 
     | PROTECTED                -- protected
     | PRIVATE                  -- private
     | STATIC                   -- static

     | EXTENDS                  -- extends

     | CLASS                    -- class
     | THIS                     -- this
     | SUPER

     | ASSIGN                   -- =
     | NEW                      -- new

     | CHAR                     -- char
     | VOID                     -- void
     | BOOLEAN                  -- boolean
     | INT                      -- int
     | STRING                   -- String (type)

     | IF                       -- if
     | WHILE                    -- while 
     | ELSE                     -- else

     | RETURN                   -- return 

     | EQUAL                    -- == 
     | NOTEQUAL                 -- !=
     | LESSEQUAL                -- <=
     | GREATEREQUAL             -- >=

     | AND                      -- &&
     | OR                       -- ||

     | BOOLLITERAL Bool         -- true | false
     | CHARLITERAL Char         -- '..'
     | STRINGLITERAL String     -- ".."  (leads to an error while scanning)
     | JNULL                    -- null

     | NEWLINE                  -- temporally holds information about positions
     | SPACE                    -- also for positioning
     | WRONGTOKEN String Int    -- error token with message and original size
     deriving (Show, Eq)


-- represents Token with line-number in source-file
data PositionedToken = PositionedToken
 { position :: Position, token :: Token }

instance Show PositionedToken where
     show (PositionedToken pos tokn) = " token: " ++ show tokn ++ " at: " ++ show pos
