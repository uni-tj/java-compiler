module Parser.Parser(parser) where

import Types.Core
import Types.AST 
import Parser.Combinators
import Scanner.Token
import Scanner.Lexer
import Data.Maybe

--temporary placeholder for positions in AST
dummyPos :: Position 
dummyPos = Position {line = 0}

{-----------------------------------------------------------------------------}
{- # "Meta" functions -}
{-----------------------------------------------------------------------------}

parser :: String -> Program
parser = tokenParser . lexWithoutIndex

tokenParser :: [Token] -> Program 
tokenParser input = fst(head(correctSols(parseProgram input)))

correctSols :: [(t, [a])] -> [(t, [a])]
correctSols sols = (filter (\(_, resttokens) -> null resttokens)) sols

{-----------------------------------------------------------------------------}
{- # Program Definition ------------------------------------------------------}
{-----------------------------------------------------------------------------}


parseProgram :: Parser Token Program
parseProgram = many parseClass

{-----------------------------------------------------------------------------}
{- # Class Definitions -------------------------------------------------------}
{-----------------------------------------------------------------------------}

--standard supertype: "Object" if not declared otherwise
parseClass :: Parser Token Types.AST.Class
parseClass =    ((parseVisibility +.+ (lexem CLASS) +.+ parseClassName +.+ (lexem EXTENDS) +.+ parseClassName +.+ (lexem LBRACKET) +.+ parseClassBody +.+ (lexem RBRACKET))
                <<< (\(vis, (_, (name, (_ , (supName, (_, (cblock,_)))))))
                    -> Types.AST.Class { cvisibility = vis, cname = name, cextends = supName,
                               cfields = getFields cblock, cmethods = getMethods cblock}))

            ||| ((parseVisibility +.+ (lexem CLASS) +.+ parseClassName +.+ (lexem LBRACKET) +.+ parseClassBody +.+ (lexem RBRACKET) ) 
                <<< (\(vis, (_, (name, (_, (cblock,_)))))
                    -> Types.AST.Class { cvisibility = vis, cname = name, cextends = "Object", 
                               cfields = getFields cblock, cmethods = getMethods cblock}))
            

--functions to parse class methods and class fields

data ClassEntry = ClassMethod Method | ClassField Field deriving Show

getFields :: [ClassEntry] -> [Field]
getFields [] = [] 
getFields ((ClassMethod mthd) : xs) = (getFields xs) 
getFields ((ClassField fld) : xs) = fld : (getFields xs) 

getMethods :: [ClassEntry] -> [Method]
getMethods [] = [] 
getMethods ((ClassMethod mthd) : xs) = mthd : (getMethods xs)
getMethods ((ClassField att) : xs) = (getMethods xs)

parseClassBody :: Parser Token [ClassEntry]
parseClassBody = many parseClassEntry


parseClassEntry :: Parser Token ClassEntry
parseClassEntry =  (parseMethodDecl <<< \mthd -> (ClassMethod mthd))
               ||| (parseFieldDecl <<< \fld -> (ClassField fld))

{-----------------------------------------------------------------------------}
{- # Methods -----------------------------------------------------------------}
{-----------------------------------------------------------------------------}

staticParser :: Parser Token Bool -- rename to parseStatic
staticParser = ((lexem STATIC) <<< (\_ -> True)) ||| (succeed False)

parseMethodDecl :: Parser Token Method
parseMethodDecl =     ((parseVisibility +.+ staticParser +.+ parseType +.+ parseMethodName +.+ (lexem LBRACE) 
                        +.+ parseMethodParams +.+ (lexem RBRACE) +.+ parseBlock) 
                            <<< (\(vis,(stc, (retType,( name, (_, (params, (_, block))))))) 
                                -> Method {mvisibility = vis, mtype = retType, mstatic = stc, mname = name,
                                    mparams = params, mbody = block}))

                  ||| ((parseVisibility +.+ parseMethodName +.+ (lexem LBRACE) +.+ parseMethodParams 
                        +.+ (lexem RBRACE) +.+ parseBlock)
                            <<< (\(vis,(name, (_, (params, (_, block))))) 
                                -> Method {mvisibility = vis, mtype = Void, mstatic = False, mname = name, 
                                    mparams = params, mbody = block}))


--parsing method parameters
parseMethodParams :: Parser Token [(Type, LocalName)]
parseMethodParams =     (succeed []) 
                    ||| ((parseType +.+ parseIdentifier)
                        <<< (\(tpe, name) -> [(tpe, name)]))

                    ||| ((parseType +.+ parseIdentifier +.+ (lexem COMMA) +.+ parseMethodParams)
                        <<< (\(tpe, (name, (_, rst))) -> ((tpe, name) : rst)))



{-----------------------------------------------------------------------------}
{- # Fields ------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseFieldDecl :: Parser Token Field
parseFieldDecl =     ((staticParser +.+ parseType +.+ parseFieldName +.+ (lexem ASSIGN) +.+ parseExpr +.+ (lexem SEMICOLON))
                        <<< (\(stc, (tpe, (name, (_, (expr, _))))) -> Field {ftype = tpe, fstatic = stc, fname = name, finit = Just expr}))
                 ||| ((staticParser +.+ parseType +.+ parseFieldName +.+ (lexem SEMICOLON))
                        <<< (\(stc, (tpe, (name, _))) -> Field {ftype = tpe, fstatic = stc, fname = name, finit = Nothing}))

{-----------------------------------------------------------------------------}
{- # Statements --------------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseStmt :: Parser Token Stmt 
parseStmt =     parseReturn
            ||| parseBlock 
            ||| parseWhile 
            ||| parseLocalVarDecl
            ||| parseIf 
            ||| parseStmtOrExprAsStmt

parseStmts :: Parser Token [Stmt]
parseStmts = many parseStmt 

parseBlock :: Parser Token Stmt
parseBlock = ((lexem LBRACKET) +.+ parseStmts +.+ (lexem RBRACKET)) <<< (\(_, (stmts, _)) -> Block dummyPos stmts)

parseReturn :: Parser Token Stmt
parseReturn =     (((lexem RETURN) +.+ (lexem SEMICOLON)) <<< (\_ -> (Return dummyPos Nothing)))
              ||| (((lexem RETURN) +.+ parseExpr +.+ (lexem SEMICOLON)) <<< (\(_, (expr, _)) -> (Return dummyPos (Just expr))))

parseWhile :: Parser Token Stmt 
parseWhile = (((lexem WHILE) +.+ parseExpr +.+ parseStmt) <<< (\(_, (expr, stmt)) -> (While dummyPos expr stmt)))


-- note: parameter of (INTLITERAL __) is ignored
parseLocalVarDecl :: Parser Token Stmt 
parseLocalVarDecl = ((parseType +.+ parseLocalName +.+ (lexem SEMICOLON)) 
                        <<< (\(tpe, (name, _)) -> (LocalVarDecl dummyPos tpe name Nothing)))
                ||| ((parseType +.+ parseLocalName +.+ (lexem ASSIGN) +.+ parseExpr +.+ (lexem SEMICOLON))
                        <<< (\(tpe, (name, (_, (expr, _)))) -> (LocalVarDecl dummyPos tpe name (Just expr))))

parseIf :: Parser Token Stmt 
parseIf =     (((lexem IF) +.+ parseExpr +.+ parseStmt) <<< (\(_, (expr, stmt)) -> (If dummyPos expr stmt Nothing)))
          ||| (((lexem IF) +.+ parseExpr +.+ parseStmt +.+ (lexem ELSE) +.+ parseStmt) <<< (\(_, (expr, (stmt1, (_, (stmt2)))))
                -> (If dummyPos expr stmt1 (Just stmt2))))

parseStmtOrExprAsStmt :: Parser Token Stmt 
parseStmtOrExprAsStmt = (parseStmtOrExpr +.+ (lexem SEMICOLON)) <<< (\(stOrEx, _) -> (StmtOrExprAsStmt dummyPos stOrEx))

{-----------------------------------------------------------------------------}
{- # StmtOrExpr --------------------------------------------------------------}
{-----------------------------------------------------------------------------}


parseStmtOrExpr :: Parser Token StmtOrExpr

parseStmtOrExpr =     parseAssignment
                  ||| parseNew 
                  ||| parseMethodCall 

--left of dot only name as expression allowed
parseAssignment :: Parser Token StmtOrExpr 
parseAssignment =     (((parseLocalName +.+ (lexem ASSIGN) +.+ parseExpr)) <<< (\(varName, (_, (expr))) -> (Assign Nothing varName expr)))
                  ||| (((parseLocalOrFieldOrClassName +.+ (lexem DOT) +.+ parseFieldName +.+ (lexem ASSIGN) +.+ parseExpr))
                        <<< (\(lfcname, (_, (fn, (_, (expr2))))) -> (Assign (Just (Name dummyPos lfcname)) fn expr2)))

parseNew :: Parser Token StmtOrExpr
parseNew = ((lexem NEW) +.+ parseClassName +.+ (lexem LBRACE) +.+ parseCallParams +.+ (lexem RBRACE))
            <<< (\(_, (name, (_, (callParams, _)))) -> (New name callParams))


parseMethodCall :: Parser Token StmtOrExpr
parseMethodCall = (((parseMethodName +.+ (lexem LBRACE) +.+ parseCallParams +.+ (lexem RBRACE)))
                    <<< (\(name, (_, (callParams, _))) -> (MethodCall Nothing name callParams)))
                ||| (((parseExpr +.+ (lexem DOT) +.+ parseMethodName +.+ (lexem LBRACE) 
                       +.+ parseCallParams +.+ (lexem RBRACE)))
                            <<< (\(expr, (_, (mname, (_, (callParams, _))))) 
                                    -> (MethodCall (Just expr) mname callParams)))


parseCallParams :: Parser Token [Expr]
parseCallParams =     succeed [] 
              ||| (parseExpr <<< (\expr -> [expr]))
              ||| ((parseExpr +.+ (lexem COMMA) +.+ parseCallParams ) <<< (\(expr, (_, exprs)) -> (expr : exprs)))

{-----------------------------------------------------------------------------}
{- # Expression --------------------------------------------------------------}
{-----------------------------------------------------------------------------}

data RightSideExpr  = RSbExpr BinOperator Expr --binop:Expr
                    | RSfaExpr String 
                    | RSmc String [Expr]

resolveRightSideExpr :: Expr -> RightSideExpr -> Expr 
resolveRightSideExpr expr (RSbExpr binop rexpr) = Binary dummyPos binop expr rexpr
resolveRightSideExpr expr (RSfaExpr fname) = FieldAccess dummyPos expr fname
resolveRightSideExpr expr (RSmc mname callParams) = StmtOrExprAsExpr dummyPos (MethodCall (Just expr) mname callParams)

parseTExpr :: Parser Token Expr
parseTExpr =    parseThis 
            ||| parseSuper 
            ||| parseName
            ||| parseUnary 
            ||| parseLiteral
            ||| parseStmtOrExprEasy
            ||| parseBraceExpr

parseExpr :: Parser Token Expr 
parseExpr =     ((parseTExpr +.+ parseExpr') <<< (\(texpr, rsexpr) -> (resolveRightSideExpr texpr rsexpr)))
            ||| parseTExpr

parseExpr' :: Parser Token RightSideExpr
parseExpr' =     (((parseBinOp +.+ parseExpr) <<< \(binop, rexpr) -> (RSbExpr binop rexpr)))
             ||| (((lexem DOT) +.+ parseFieldName) <<< \(_, fname) -> (RSfaExpr fname))
             ||| (((lexem DOT) +.+ parseMethodName +.+ (lexem LBRACE) +.+ parseCallParams +.+ (lexem RBRACE))
                    <<< (\(_, (mname, (_, (callParams, _)))) -> (RSmc mname callParams)))

parseThis :: Parser Token Expr 
parseThis = (lexem THIS) <<< (\_ -> (This dummyPos))

parseSuper :: Parser Token Expr 
parseSuper = (lexem SUPER) <<< (\_ -> (Super dummyPos))

parseName :: Parser Token Expr
parseName =  parseLocalName <<< (\ln -> (Name dummyPos ln))

parseFieldAccess :: Parser Token Expr
parseFieldAccess = ((parseExpr +.+ (lexem DOT) +.+ parseFieldName))
                        <<< (\(expr, (_, fn)) -> (FieldAccess dummyPos expr fn))
parseUnary :: Parser Token Expr 
parseUnary = (parseUnOp +.+ parseExpr) <<< (\(op, expr) -> (Unary dummyPos op expr))

parseLiteral :: Parser Token Expr 
parseLiteral = parseLiteralAsLiteral <<< \lit -> (Literal dummyPos lit)


parseBraceExpr :: Parser Token Expr 
parseBraceExpr = ((lexem LBRACE) +.+ parseExpr +.+ (lexem RBRACE)) <<< \(_, (expr, _)) -> expr 

parseStmtOrExprEasy :: Parser Token Expr 
parseStmtOrExprEasy = ((parseMethodName +.+ (lexem LBRACE) +.+ parseCallParams +.+ (lexem RBRACE))
                        <<< (\(mname,(_, (callParams,_))) -> StmtOrExprAsExpr dummyPos (MethodCall Nothing mname callParams)))
                    ||| (parseNew <<< \new -> (StmtOrExprAsExpr dummyPos new))
                    ||| (parseAssignment <<< \asgnmnt -> (StmtOrExprAsExpr dummyPos asgnmnt))

parseUnOp :: Parser Token UnOparator -- still typo
parseUnOp =     ((lexem PLUS) <<< (\_ -> Plus))
            ||| ((lexem MINUS) <<< (\_ -> Minus))
            ||| ((lexem INCREMENT) <<< (\_ -> PreIncrement))
            ||| ((lexem DECREMENT) <<< (\_ -> PreDecrement))
            ||| ((lexem EXCLMARK) <<< (\_ -> LNot))

parseBinOp :: Parser Token BinOperator
parseBinOp =     ((lexem PLUS) <<< (\_ -> Add))
             ||| ((lexem MINUS) <<< (\_ -> Sub))
             ||| ((lexem MUL) <<< (\_ -> Mul))
             ||| ((lexem DIV) <<< (\_ -> Div))
             ||| ((lexem MOD) <<< (\_ -> Mod))
             ||| ((lexem AND) <<< (\_ -> LAnd))
             ||| ((lexem OR) <<< (\_ -> Types.Core.LOr))
             ||| ((lexem LESS) <<< (\_ -> Types.Core.LT))
             ||| ((lexem LESSEQUAL) <<< (\_ -> Types.Core.LTE))
             ||| ((lexem GREATER) <<< (\_ -> Types.Core.GT))
             ||| ((lexem GREATEREQUAL) <<< (\_ -> GTE))
             ||| ((lexem EQUAL) <<< (\_ -> Types.Core.EQ))
             ||| ((lexem NOTEQUAL) <<< (\_ -> Types.Core.NEQ))


--placeholders for arguments, that are irrelevant
intLit :: Integer 
intLit = 0 

charLit :: Char 
charLit = 'a'

boolLit :: Bool
boolLit = True

anyString :: String 
anyString = "anything"

parseLiteralAsLiteral :: Parser Token Literal
parseLiteralAsLiteral =     (((lexemParam (INTLITERAL intLit)) <<< (\(INTLITERAL x) -> (IntLit x))))
                        ||| (((lexemParam (CHARLITERAL charLit)) <<< (\(CHARLITERAL chr) -> (CharLit chr))))
                        ||| (((lexemParam (BOOLLITERAL boolLit)) <<< (\(BOOLLITERAL bol) -> (BoolLit bol))))
                        ||| (((lexem JNULL) <<< (\_ -> Null)))

{-----------------------------------------------------------------------------}
{- # Types -------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- parameter of type (IDENTIFIER __) are ignored
parseType :: Parser Token Type
parseType =     ((lexem CHAR) <<< (\_ -> Char))
            ||| ((lexem INT) <<< (\_ -> Int))
            ||| ((lexem BOOLEAN) <<< (\_ -> Bool))
            ||| ((lexem VOID) <<< (\_ -> Void))
            ||| ((lexemParam (IDENTIFIER anyString)) <<< (\(IDENTIFIER name) -> (Types.Core.Class name)))
            ||| (((lexem STRING) +.+ (lexem LSQRBRACKET) +.+ (lexem RSQRBRACKET)) <<< (\_ -> StringArr))

{-----------------------------------------------------------------------------}
{- # Visibilities ------------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseVisibility :: Parser Token Visibility 
parseVisibility =     ((lexem PUBLIC) <<< (\_ -> Public))
                  ||| ((lexem PRIVATE) <<< (\_ -> Private))
                  ||| ((lexem PROTECTED) <<< (\_ -> Package)) -- maybe in future: package
                  ||| succeed Package                         -- default: package

{-----------------------------------------------------------------------------}
{- # Identifier names --------------------------------------------------------}
{-----------------------------------------------------------------------------}


parseIdentifier :: Parser Token Identifier
parseIdentifier = ((lexemParam (IDENTIFIER anyString)) <<< (\(IDENTIFIER str) -> str))

parseClassName :: Parser Token ClassName 
parseClassName = parseIdentifier 

parseFieldName :: Parser Token FieldName 
parseFieldName = parseIdentifier 

parseMethodName :: Parser Token MethodName 
parseMethodName = parseIdentifier

parseLocalName :: Parser Token LocalName 
parseLocalName = parseIdentifier 

parseLocalOrFieldOrClassName :: Parser Token LocalOrFieldOrClassName 
parseLocalOrFieldOrClassName = parseIdentifier