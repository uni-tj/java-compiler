
module Parser.IndexParser(parser) where

import Types.Core
    ( AccessModifier(Package, Public, Private),
      BinOperator(..),
      ClassName,
      FieldName,
      Identifier,
      LocalName,
      LocalOrFieldOrClassName,
      MethodName,
      Type(StringArr, Instance, Char, Int, Bool, Void, Class),
      UnOparator(..) )
import Types.AST
    ( Position(..),
      Program,
      Class(..),
      Field(..),
      Method(..),
      Stmt(..),
      Expr(..),
      StmtOrExpr(..),
      Literal(..) )
import Parser.Combinators
import Scanner.Token
import Scanner.Lexer
import Data.Either (lefts, rights)

-- computing the position spanning from position 1 to position 2
spanPos :: Position -> Position -> Position
spanPos pos1 pos2 = Position { start = start pos1, end = end pos2}

-- computing the position spanning from tkn1 to tkn2
makePos :: PositionedToken -> PositionedToken -> Position
makePos tkn1 tkn2 = spanPos (position tkn1) (position tkn2)

getPosFromStmt :: Stmt -> Position
getPosFromStmt stmt = case stmt of
    (Block pos _) -> pos
    (Return pos _) -> pos
    (LocalVarDecl pos _ _ _) -> pos
    (If pos _ _ _) -> pos
    (StmtOrExprAsStmt pos _) -> pos
    (While pos _ _) -> pos

getPosFromExpr :: Expr -> Position
getPosFromExpr expr = case expr of
    (This pos) -> pos
    (Super pos) -> pos
    (Name pos _) -> pos
    (FieldAccess pos _ _) -> pos
    (Unary pos _ _) -> pos
    (Binary pos _ _ _) -> pos
    (Literal pos _) -> pos
    (StmtOrExprAsExpr pos _) -> pos


{-----------------------------------------------------------------------------}
{-- # main functionalities -}
{-----------------------------------------------------------------------------}

parser :: String -> Program
parser = tokenParser . lexWithIndex

tokenParser :: [PositionedToken] -> Program
tokenParser input = fst (head (correctSols (parseProgram input)))

correctSols :: [(t, [a])] -> [(t, [a])]
correctSols = filter (\(_, resttokens) -> null resttokens)

{-----------------------------------------------------------------------------}
{-- # Program Definition -----------------------------------------------------}
{-----------------------------------------------------------------------------}


parseProgram :: Parser PositionedToken Program
parseProgram = many parseClass

{-----------------------------------------------------------------------------}
{-- # Class Definitions ------------------------------------------------------}
{-----------------------------------------------------------------------------}

--standard supertype: "Object" if not declared otherwise
parseClass :: Parser PositionedToken Types.AST.Class
parseClass =    ((parseVisibility +.+ posLexem CLASS +.+ parseClassName +.+ posLexem EXTENDS +.+ parseClassName +.+ posLexem LBRACKET +.+ parseClassBody +.+ posLexem RBRACKET)
                <<< (\(vis, (_, (name, (_ , (supName, (_, (cblock,_)))))))
                    -> Types.AST.Class { caccess = vis, cname = fst name, cextends = fst supName,
                               cfields = getFields cblock, cmethods = getMethods cblock}))

            ||| ((parseVisibility +.+ posLexem CLASS +.+ parseClassName +.+ posLexem LBRACKET +.+ parseClassBody +.+ posLexem RBRACKET )
                <<< (\(vis, (_, (name, (_, (cblock,_)))))
                    -> Types.AST.Class { caccess = vis, cname = fst name, cextends = "Object",
                               cfields = getFields cblock, cmethods = getMethods cblock}))

getFields :: [Either Field Method] -> [Field]
getFields = lefts

getMethods :: [Either Field Method] -> [Method]
getMethods = rights

parseClassBody :: Parser PositionedToken [Either Field Method]
parseClassBody = many parseClassEntry

-- parsing either field-declaration or method-declaration
parseClassEntry :: Parser PositionedToken (Either Field Method)
parseClassEntry =  (parseMethodDecl <<< Right)
               ||| (parseFieldDecl <<< Left)

{-----------------------------------------------------------------------------}
{-- # Methods ----------------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseStatic :: Parser PositionedToken Bool -- rename to parseStatic
parseStatic = (posLexem STATIC <<< const True) ||| succeed False

parseMethodDecl :: Parser PositionedToken Method
parseMethodDecl =     ((parseVisibility +.+ parseStatic +.+ parseType +.+ parseMethodName +.+ posLexem LBRACE -- parsing regular method decl
                        +.+ parseMethodParams +.+ posLexem RBRACE +.+ parseBlock)
                            <<< (\(vis,(stc, ((retType, _),((name, _), (_, (params, (_, block)))))))
                                -> Method {maccess = vis, mtype = retType, mstatic = stc, mname = name,
                                    mparams = params, mbody = block}))

                  ||| ((parseVisibility +.+ parseMethodName +.+ posLexem LBRACE +.+ parseMethodParams   -- parsing a constructor
                        +.+ posLexem RBRACE +.+ parseBlock)
                            <<< (\(vis,((name, _), (_, (params, (_, block)))))
                                -> Method {maccess = vis, mtype = Instance name, mstatic = False, mname = name,
                                    mparams = params, mbody = block}))


--parsing method parameters
parseMethodParams :: Parser PositionedToken [(Type, LocalName)]
parseMethodParams =     succeed []
                    ||| ((parseType +.+ parseIdentifier)
                        <<< (\((tpe, _), (name, _)) -> [(tpe, name)]))

                    ||| ((parseType +.+ parseIdentifier +.+ posLexem COMMA +.+ parseMethodParams)
                        <<< (\((tpe, _), ((name, _), (_, rst))) -> (tpe, name) : rst))



{-----------------------------------------------------------------------------}
{-- # Fields -----------------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseFieldDecl :: Parser PositionedToken Field
parseFieldDecl =     ((parseVisibility +.+ parseStatic +.+ parseType +.+ parseFieldName +.+ posLexem ASSIGN +.+ parseExpr +.+ posLexem SEMICOLON)
                        <<< (\(vis, (stc, (tpe, (name, (_, (expr, _))))))
                            -> Field {faccess = vis, ftype = fst tpe, fstatic = stc, fname = fst name, finit = Just expr}))

                 ||| ((parseVisibility +.+ parseStatic +.+ parseType +.+ parseFieldName +.+ posLexem SEMICOLON)
                        <<< (\(vis, (stc, (tpe, (name, _))))
                            -> Field {faccess = vis, ftype = fst tpe, fstatic = stc, fname = fst name, finit = Nothing}))

{-----------------------------------------------------------------------------}
{-- # Statements -------------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseStmt :: Parser PositionedToken Stmt
parseStmt =     parseReturn
            ||| parseBlock
            ||| parseWhile
 --         ||| parseForLoop
            ||| parseLocalVarDecl
            ||| parseIf
            ||| parseStmtOrExprAsStmt
            ||| parseThisOrSuperCall

{-
parseSingletonStmt :: Parser PositionedToken Stmt
parseSingletonStmt =     parseLocalVarDecl
                     ||| (parseStmtOrExpr <<< \(soe,pos) -> StmtOrExprAsStmt pos soe)
-}

parseStmts :: Parser PositionedToken [Stmt]
parseStmts = many parseStmt

parseBlock :: Parser PositionedToken Stmt
parseBlock = (posLexem LBRACKET +.+ parseStmts +.+ posLexem RBRACKET)
                <<< (\(lb, (stmts, rb)) -> Block (makePos lb rb) stmts)

parseReturn :: Parser PositionedToken Stmt
parseReturn =     ((posLexem RETURN +.+ posLexem SEMICOLON)
                    <<< (\(ret, semicolon) -> Return (makePos ret semicolon) Nothing))

              ||| ((posLexem RETURN +.+ parseExpr +.+ posLexem SEMICOLON)
                    <<< (\(ret, (expr, semicolon)) -> Return (makePos ret semicolon) (Just expr)))

parseWhile :: Parser PositionedToken Stmt
parseWhile = (posLexem WHILE +.+ parseExpr +.+ parseStmt)
                <<< (\(while, (expr, stmt))
                    -> While (spanPos (position while) (getPosFromStmt stmt)) expr stmt)
{-
parseForLoop :: Parser PositionedToken Stmt
parseForLoop = (posLexem FOR +.+ posLexem LBRACE +.+ parseSingletonStmt +.+ posLexem SEMICOLON +.+ parseExpr
                +.+ posLexem SEMICOLON +.+ parseSingletonStmt +.+ posLexem RBRACE +.+ parseStmt)
                    <<< \(for, (_, (stmt1, (_, (bexpr, (_, (stmt3, (_, block))))))))
                        ->  let pos = spanPos (position for) (getPosFromStmt block)
                                blockPos = getPosFromStmt block in
                                Block pos [stmt1, While pos bexpr (Block blockPos [block,stmt3])]
-}

-- note: parameter of (INTLITERAL __) is ignored
parseLocalVarDecl :: Parser PositionedToken Stmt
parseLocalVarDecl =     ((parseType +.+ parseLocalName +.+ posLexem SEMICOLON)
                            <<< (\((tpe, pos1), ((name, _), semicolon))
                                -> LocalVarDecl (spanPos pos1 (position semicolon)) tpe name Nothing))

                    ||| ((parseType +.+ parseLocalName +.+ posLexem ASSIGN +.+ parseExpr +.+ posLexem SEMICOLON)
                            <<< (\((tpe, pos1), ((name, _), (_, (expr, semicolon))))
                                -> LocalVarDecl (spanPos pos1 (position semicolon)) tpe name (Just expr)))

parseIf :: Parser PositionedToken Stmt
parseIf =     ((posLexem IF +.+ parseExpr +.+ parseStmt)
                <<< (\(ifname, (expr, stmt))
                    -> If (spanPos (position ifname) (getPosFromStmt stmt)) expr stmt Nothing))

          ||| ((posLexem IF +.+ parseExpr +.+ parseStmt +.+ posLexem ELSE +.+ parseStmt)
                <<< (\(ifname, (expr, (stmt1, (_, stmt2))))
                    -> If (spanPos (position ifname) (getPosFromStmt stmt2)) expr stmt1 (Just stmt2)))

parseStmtOrExprAsStmt :: Parser PositionedToken Stmt
parseStmtOrExprAsStmt = (parseStmtOrExpr +.+ posLexem SEMICOLON) <<< (\((stOrEx, pos1), semicolon) -> StmtOrExprAsStmt (spanPos pos1 (position semicolon)) stOrEx)

parseThisOrSuperCall :: Parser PositionedToken Stmt 
parseThisOrSuperCall = ||| (parsePosLexem THIS +.+ parsePosLexem LBRACE +.+ parseCallParams +.+ parsePosLexem RBRACE) 
                            <<< \(this, (_, (params, rb))) -> ThisCall pos
                            
                       ||| (parsePosLexem SUPER +.+ parsePosLexem LBRACE +.+ parseCallParams +.+ parsePosLexem RBRACE)
                            <<< \(super, (_, (params, rb))) -> SuperCall 

{-----------------------------------------------------------------------------}
{-- # StmtOrExpr -------------------------------------------------------------}
{-----------------------------------------------------------------------------}


parseStmtOrExpr :: Parser PositionedToken (StmtOrExpr, Position)

parseStmtOrExpr =     parseAssignment
                  ||| parseNew
                  ||| parseMethodCall

--left of dot only name as expression allowed
parseAssignment :: Parser PositionedToken (StmtOrExpr, Position)
parseAssignment =     ((parseLocalName +.+ posLexem ASSIGN +.+ parseExpr)
                        <<< (\((varName, pos1), (_, expr)) -> (Assign Nothing varName expr, spanPos pos1 (getPosFromExpr expr))))

                  ||| ((parseExpr +.+ posLexem DOT +.+ parseFieldName +.+ posLexem ASSIGN +.+ parseExpr)
                        <<< (\(expr1, (_, ((fn, _), (_, expr2)))) -> (Assign (Just expr1) fn expr2, spanPos (getPosFromExpr expr1) (getPosFromExpr expr2))))

parseNew :: Parser PositionedToken (StmtOrExpr, Position)
parseNew = (posLexem NEW +.+ parseClassName +.+ posLexem LBRACE +.+ parseCallParams +.+ posLexem RBRACE)
            <<< (\(new, ((name, _), (_, (callParams, rb)))) -> (New name callParams, makePos new rb))


parseMethodCall :: Parser PositionedToken (StmtOrExpr, Position)
parseMethodCall = ((parseMethodName +.+ posLexem LBRACE +.+ parseCallParams +.+ posLexem RBRACE)
                    <<< (\((name, pos1), (_, (callParams, rb)))
                        -> (MethodCall Nothing name callParams, spanPos pos1 (position rb))))

                ||| ((parseExpr +.+ posLexem DOT +.+ parseMethodName +.+ posLexem LBRACE +.+ parseCallParams +.+ posLexem RBRACE)
                    <<< (\(expr, (_, ((mthName,_), (_, (callParams, rb)))))
                        -> (MethodCall (Just expr) mthName callParams, spanPos (getPosFromExpr expr) (position rb))))


parseCallParams :: Parser PositionedToken [Expr]
parseCallParams =      succeed []
                  ||| (parseExpr <<< (: []))
                  ||| ((parseExpr +.+ posLexem COMMA +.+ parseCallParams )
                        <<< (\(expr, (_, exprs)) -> expr : exprs))

{-----------------------------------------------------------------------------}
{-- # Expression -------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- temporary type to hold the right side of an expression (where left rec would occur)
data RightSideExpr  = RSbExpr BinOperator Expr -- Right Side binary expression
                    | RSfaExpr Position String          -- Right Side field access
                    | RSmc Position String [Expr]       -- Right Side method call 
                    | RSassign Position String Expr     -- Right Side Assign
                    | Chain RightSideExpr RightSideExpr -- chain of field-accesses a.b.c....

-- evals to true, if first opeartor binds more than second one
binopCompare :: BinOperator -> BinOperator -> Bool
binopCompare op1 op2 = mapBinopPrecedence op1 < mapBinopPrecedence op2

-- a smaller value means the operator binds stronger
mapBinopPrecedence :: BinOperator -> Int
mapBinopPrecedence op = case op of
        Mul -> 1
        Div -> 1
        Mod -> 1
        Add -> 2
        Sub -> 2
        Types.Core.LT -> 3
        LTE -> 3
        Types.Core.GT -> 3
        GTE -> 3
        Types.Core.EQ -> 4
        NEQ -> 4
        LAnd -> 5
        LOr -> 6

-- deconstruction of the right side of an expression (with correct operator presedence)
resolveRightSideExpr :: Expr -> RightSideExpr -> Expr

resolveRightSideExpr expr (RSbExpr lop (Binary pos rop ls rs)) | binopCompare lop rop = Binary (spanPos (getPosFromExpr expr) pos) rop (Binary pos lop expr ls) rs
                                                               | otherwise = Binary (spanPos (getPosFromExpr expr) pos) lop expr (Binary pos rop ls rs)
resolveRightSideExpr expr rightSideExpr = case rightSideExpr of
        (RSbExpr binop rexpr) -> Binary (spanPos (getPosFromExpr expr) (getPosFromExpr rexpr)) binop expr rexpr
        (RSfaExpr pos2 fldName) -> FieldAccess (spanPos (getPosFromExpr expr) pos2) expr fldName
        (RSmc pos2 mthName callParams) -> StmtOrExprAsExpr (spanPos (getPosFromExpr expr) pos2) (MethodCall (Just expr) mthName callParams)
        (RSassign pos2 fldName rexpr) -> StmtOrExprAsExpr (spanPos (getPosFromExpr expr) pos2) (Assign (Just expr) fldName rexpr)
        (Chain rsExpr1 rsExpr2) -> resolveRightSideExpr (resolveRightSideExpr expr rsExpr1) rsExpr2

--terminating expressions
parseTExpr :: Parser PositionedToken Expr
parseTExpr =    parseThis
            ||| parseSuper
            ||| parseName
            ||| parseUnary
            ||| parseLiteralExpr
            ||| parseStmtOrExprEasy
            ||| parseBraceExpr

-- general expressions
parseExpr :: Parser PositionedToken Expr
parseExpr =     ((parseTExpr +.+ parseExpr') <<< uncurry resolveRightSideExpr)
             ||| parseTExpr

-- right side expressions
parseExpr' :: Parser PositionedToken RightSideExpr
parseExpr' =     ((parseBinOp +.+ parseExpr)                                                                         -- + expr
                    <<< uncurry RSbExpr)
             ||| ((posLexem DOT +.+ parseFieldName)                                                                  -- .field Access
                    <<< \(dot, (fldName, pos)) -> RSfaExpr (spanPos (position dot) pos) fldName)
             ||| ((posLexem DOT +.+ parseMethodName +.+ posLexem LBRACE +.+ parseCallParams +.+ posLexem RBRACE)     -- .method call
                    <<< (\(dot, ((mthName, _), (_, (callParams, rb)))) -> RSmc (makePos dot rb) mthName callParams))
             ||| ((posLexem DOT +.+ parseFieldName +.+ posLexem ASSIGN +.+ parseExpr)                                -- .assignment
                    <<< \(dot, ((fldName, _) , (_, expr))) -> RSassign (spanPos (position dot) (getPosFromExpr expr)) fldName expr)
             ||| ((posLexem DOT +.+ parseFieldName +.+ parseExpr')                                                   -- special case: chain .a.b.c... | .a + b
                    <<< \(dot, ((fldName, pos), texpr)) -> Chain (RSfaExpr (spanPos (position dot) pos) fldName) texpr)

parseThis :: Parser PositionedToken Expr
parseThis = posLexem THIS <<< \tkn ->  This (position tkn)

parseSuper :: Parser PositionedToken Expr
parseSuper = posLexem SUPER <<< \tkn -> Super (position tkn)

parseName :: Parser PositionedToken Expr
parseName =  parseLocalOrFieldOrClassName <<< \(name, pos) -> Name pos name

parseUnary :: Parser PositionedToken Expr
parseUnary = (parseUnOp +.+ parseExpr) <<< \(op, expr) -> Unary (getPosFromExpr expr) op expr

parseLiteralExpr :: Parser PositionedToken Expr
parseLiteralExpr = parseLiteral <<< \(lit, pos) -> Literal pos lit

parseBraceExpr :: Parser PositionedToken Expr
parseBraceExpr = (posLexem LBRACE +.+ parseExpr +.+ posLexem RBRACE) <<< \(_, (expr, _)) -> expr

--parse stmt or expr without left recursion
parseStmtOrExprEasy :: Parser PositionedToken Expr
parseStmtOrExprEasy = ((parseMethodName +.+ posLexem LBRACE +.+ parseCallParams +.+ posLexem RBRACE) -- parse method call without expression
                        <<< (\((mthName, pos1),(_, (callParams,rb)))
                            -> StmtOrExprAsExpr (spanPos pos1 (position rb)) (MethodCall Nothing mthName callParams)))

                    ||| (parseNew <<< \(new, pos) -> StmtOrExprAsExpr pos new) -- parse 'new' expression

                    ||| ((parseLocalName +.+ posLexem ASSIGN +.+ parseExpr)
                        <<< (\((varName, pos1), (_, expr))
                            -> StmtOrExprAsExpr (spanPos pos1 (getPosFromExpr expr)) (Assign Nothing varName expr))) --parse simple assignment

parseUnOp :: Parser PositionedToken UnOparator -- still typo
parseUnOp =     (posLexem PLUS          <<< const Plus)
            ||| (posLexem MINUS         <<< const Minus)
            ||| (posLexem EXCLMARK      <<< const LNot)

parseBinOp :: Parser PositionedToken BinOperator
parseBinOp =     (posLexem PLUS         <<< const Add)
             ||| (posLexem MINUS        <<< const Sub)
             ||| (posLexem MUL          <<< const Mul)
             ||| (posLexem DIV          <<< const Div)
             ||| (posLexem MOD          <<< const Mod)
             ||| (posLexem AND          <<< const LAnd)
             ||| (posLexem OR           <<< const LOr)
             ||| (posLexem LESS         <<< const Types.Core.LT)
             ||| (posLexem LESSEQUAL    <<< const LTE)
             ||| (posLexem GREATER      <<< const Types.Core.GT)
             ||| (posLexem GREATEREQUAL <<< const GTE)
             ||| (posLexem EQUAL        <<< const Types.Core.EQ)
             ||| (posLexem NOTEQUAL     <<< const NEQ)

-- placeholders for irrelevant arguments
intLit :: Integer
intLit = 0

charLit :: Char
charLit = 'a'

boolLit :: Bool
boolLit = True

anyString :: String
anyString = "anything"

parseLiteral :: Parser PositionedToken (Literal, Position)
parseLiteral =      (posLexemParam (INTLITERAL intLit)
                        <<< (\PositionedToken { token = (INTLITERAL x), position = pos} -> (IntLit x, pos)))

                ||| (posLexemParam (CHARLITERAL charLit)
                        <<< (\PositionedToken { token = (CHARLITERAL chr), position = pos} -> (CharLit chr, pos)))

                ||| (posLexemParam (BOOLLITERAL boolLit)
                        <<< (\PositionedToken {token = (BOOLLITERAL bol), position = pos} -> (BoolLit bol, pos)))

                ||| (posLexem JNULL
                        <<< (\tkn -> (Null, position tkn)))

{-----------------------------------------------------------------------------}
{-- # Types ------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- parameter of type (IDENTIFIER __) are ignored
parseType :: Parser PositionedToken (Type, Position)
parseType =     (posLexem CHAR <<< (\tkn -> (Char, position tkn)))
            ||| (posLexem INT <<< (\tkn -> (Int, position tkn)))
            ||| (posLexem BOOLEAN <<< (\tkn -> (Bool, position tkn)))
            ||| (posLexem VOID <<< (\tkn -> (Void, position tkn)))
            ||| (posLexemParam (IDENTIFIER anyString)
                    <<< \PositionedToken { token = (IDENTIFIER name), position = pos} -> (Types.Core.Class name, pos))
            ||| ((posLexem STRING +.+ posLexem LSQRBRACKET +.+ posLexem RSQRBRACKET)
                    <<< \(str, (_, rb)) -> (StringArr, makePos str rb))

{-----------------------------------------------------------------------------}
{-- # Visibilities -----------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseVisibility :: Parser PositionedToken AccessModifier
parseVisibility =     (posLexem PUBLIC <<< const Public)
                  ||| (posLexem PRIVATE <<< const Private)
                  ||| (posLexem PROTECTED <<< const Package)
                  ||| succeed Package    -- default: package

{-----------------------------------------------------------------------------}
{-- # Identifier names -------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseIdentifier :: Parser PositionedToken (Identifier, Position)
parseIdentifier = posLexemParam (IDENTIFIER anyString)
                    <<< \PositionedToken {token = (IDENTIFIER name), position = pos} -> (name, pos)

parseClassName :: Parser PositionedToken (ClassName, Position)
parseClassName = parseIdentifier

parseFieldName :: Parser PositionedToken (FieldName, Position)
parseFieldName = parseIdentifier

parseMethodName :: Parser PositionedToken (MethodName, Position)
parseMethodName = parseIdentifier

parseLocalName :: Parser PositionedToken (LocalName, Position)
parseLocalName = parseIdentifier

parseLocalOrFieldOrClassName :: Parser PositionedToken (LocalOrFieldOrClassName, Position)
parseLocalOrFieldOrClassName = parseIdentifier