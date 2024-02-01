module Parser.ErrorParser where 

import Types.Core 
import Types.AST
import Parser.Combinators
import Scanner.Token 
import Scanner.Lexer 




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
    (SuperCall pos _)-> pos
    (ThisCall pos _) -> pos

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

{-
--AIO parser and scanner
completeParser :: String -> Either String Program 
completeParser = parser . lexWithIndex

--parser only
parser :: [PositionedToken] -> Either String Program
parser = tokenParser

tokenParser :: [PositionedToken] -> Either String Program
tokenParser input =
    let parsed = parseProgram input
    in
        let correctSolutions =  correctSols parsed
        in 
            if null correctSolutions then Left (remaining parsed)
            else Right (fst (head (correctSols (parseProgram input))))

remaining :: Show b => [(a, [b])] -> String 
remaining ((_, [b]) : xs) = "error on tokens: " ++ show b
remaining _ = " ";

correctSols :: [(t, [a])] -> [(t, [a])]
correctSols = filter (\(_, resttokens) -> null resttokens)

-}

{-----------------------------------------------------------------------------}
{-- # Program Definition -----------------------------------------------------}
{-----------------------------------------------------------------------------}

-- an empty Program is also valid
{- 
    Program = e | Class : e
-}

parseProgram :: EParser PositionedToken Program
parseProgram = manyE parseClass

{-----------------------------------------------------------------------------}
{-- # Class Definitions ------------------------------------------------------}
{-----------------------------------------------------------------------------}

{-  # class declaration, reduces to Types.AST.Class 
    Class = Visibility : Static : 'class' : 'extends' : ClassName :  '{' : ClassBody : '}'
          | Visibility : Static : 'class' : '{' : ClassBody : '}'
-}
parseClass :: EParser PositionedToken Types.AST.Class
parseClass =    ((parseVisibility +++  item CLASS +++ parseClassName +++  item EXTENDS +++ parseClassName +++  item LBRACKET +++ parseClassBody +++  item RBRACKET)
                 >>> (\(vis, (_, (name, (_ , (supName, (_, (cblock,_)))))))
                    -> Types.AST.Class { caccess = vis, cname = fst name, cextends = Just (fst supName),
                               cfields = getFields cblock, cmethods = getMethods cblock, cconstructors = getConstructors cblock (fst name)}))

             <|> ((parseVisibility +++  item CLASS +++ parseClassName +++  item LBRACKET +++ parseClassBody +++  item RBRACKET )
                 >>> (\(vis, (_, (name, (_, (cblock,_)))))
                    -> Types.AST.Class { caccess = vis, cname = fst name, cextends = Nothing, --nothing indicating: extends Object
                               cfields = getFields cblock, cmethods = getMethods cblock, cconstructors = getConstructors  cblock (fst name)}))

--temporary data type for class entries
data ClassEntry = ClassConstructor Constructor
                | ClassMethod Method
                | ClassField Field

getFields :: [ClassEntry] -> [Field]
getFields ce = [fld | ClassField fld <- ce]

getMethods :: [ClassEntry] -> [Method]
getMethods ce = [mth | ClassMethod mth <- ce]

-- here is checked, weather the contructor has the correct name
getConstructors :: [ClassEntry] -> ClassName -> [Constructor]
getConstructors ce name = [if crname cr == name then cr else error "wrong name for constructor" | ClassConstructor cr <- ce]

{-
    ClassBody = e 
              | FieldDeclaration : ClassBody 
              | MthdDeclaration : ClassBody
              | ConstructorDeclaration : ClassBody
-}
parseClassBody :: EParser PositionedToken [ClassEntry]
parseClassBody = manyE parseClassEntry

-- parsing either field-declaration or method-declaration
parseClassEntry :: EParser PositionedToken ClassEntry
parseClassEntry =     (parseMethodDecl       >>> ClassMethod)
                   <|> (parseConstructorDecl  >>> ClassConstructor)
                   <|> (parseFieldDecl        >>> ClassField)

{-----------------------------------------------------------------------------}
{-- # Constructors -----------------------------------------------------------}
{-----------------------------------------------------------------------------}


parseConstructorDecl :: EParser PositionedToken Types.AST.Constructor
parseConstructorDecl =    ((parseVisibility +++ parseClassName +++  item LBRACE +++ parseMethodParams +++  item RBRACE +++ 
                             item LBRACKET +++ parseThisOrSuperCall +++ parseStmts +++  item RBRACKET)
                             >>> (\(vis, (name, (_, (params, (_, (lB, (thsSupCall, (body, rB))))))))
                                -> Types.AST.Constructor {craccess = vis, crname = fst name, crparams = params, crbody = Block (makePos lB rB) (thsSupCall ++ body)}))
                       <|> ((parseVisibility +++ parseClassName +++ item  LBRACE +++ parseMethodParams +++ item  RBRACE +++ parseBlock)
                             >>> (\(vis, (name, (_, (params, (_, body)))))
                                -> Types.AST.Constructor {craccess = vis, crname = fst name, crparams = params, crbody = body}))

parseThisOrSuperCall :: EParser PositionedToken  [Stmt]
parseThisOrSuperCall =    ((item  THIS +++ item  LBRACE +++ parseCallParams +++ item  RBRACE) 
                             >>> \(pos1, (_, (params, rb))) 
                                -> [ThisCall (makePos pos1 rb) params])

                        <|> ((item  SUPER +++ item  LBRACE +++ parseCallParams +++ item  RBRACE)
                             >>> \(pos1, (_, (params, rb))) 
                                -> [SuperCall (makePos pos1 rb) params])

{-----------------------------------------------------------------------------}
{-- # Methods ----------------------------------------------------------------}
{-----------------------------------------------------------------------------}

{-  static flag, reduces to bool, weather token is present or not
    Static = e | 'static'
-}
parseStatic :: EParser PositionedToken Bool -- rename to parseStatic
parseStatic = (item  STATIC  >>> const True)  <|> succeedE False

{-  override annotation, recudes to bool, weather token is present or not
    Override = e | '@Override'
-}
parseOverride :: EParser PositionedToken Bool 
parseOverride = (item  OVERRIDE  >>> const True)  <|> succeedE False

{-  method declaration, reduces to Types.AST.Method
    MthdDeclaration = Visibility : Static : Type : Name : '(' : MthdParams :  ')' : Block 
                    | Visibility : Name : '(' : MthdParams : ')' : Block 
-}
parseMethodDecl :: EParser PositionedToken Types.AST.Method
parseMethodDecl = (parseOverride +++ parseVisibility +++ parseStatic +++ parseType +++ parseMethodName +++ item  LBRACE -- parsing regular method decl
                    +++ parseMethodParams +++ item  RBRACE +++ parseBlock)
                         >>> (\(ovrFlg, (vis,(stc, ((retType, _),((name, _), (_, (params, (_, block))))))))
                            -> Method {maccess = vis, mtype = retType, mstatic = stc, mname = name,
                                mparams = params, mbody = block, moverride = ovrFlg})

{-
    MthdParams = e | Type : Name | Type : Name : ',' : MthdParams
-}
--parsing method parameters
parseMethodParams :: EParser PositionedToken [(Type, LocalName)]
parseMethodParams =     succeedE []
                     <|> ((parseType +++ parseIdentifier)
                         >>> (\((tpe, _), (name, _)) -> [(tpe, name)]))

                     <|> ((parseType +++ parseIdentifier +++ item  COMMA +++ parseMethodParams)
                         >>> (\((tpe, _), ((name, _), (_, rst))) -> (tpe, name) : rst))



{-----------------------------------------------------------------------------}
{-- # Fields -----------------------------------------------------------------}
{-----------------------------------------------------------------------------}

{-  field declaration, reduces to Types.AST.Field
    FieldDeclaration = Visibility : Static : Type : Name : "=" : Expr : ';'
                     | Visibliity : Static : Type : Name : ";"
-}
parseFieldDecl :: EParser PositionedToken Types.AST.Field
parseFieldDecl =     ((parseOverride +++ parseVisibility +++ parseStatic +++ parseType +++ parseFieldName +++ item  ASSIGN +++ parseExpr +++ item  SEMICOLON)
                         >>> (\(ovrFlg, (vis, (stc, (tpe, (name, (_, (expr, _)))))))
                            -> Field {faccess = vis, ftype = fst tpe, fstatic = stc, fname = fst name, finit = Just expr, foverride = ovrFlg}))

                  <|> ((parseOverride +++ parseVisibility +++ parseStatic +++ parseType +++ parseFieldName +++ item  SEMICOLON)
                         >>> (\(ovrFlg, (vis, (stc, (tpe, (name, _)))))
                            -> Field {faccess = vis, ftype = fst tpe, fstatic = stc, fname = fst name, finit = Nothing, foverride = ovrFlg}))

{-----------------------------------------------------------------------------}
{-- # Statements -------------------------------------------------------------}
{-----------------------------------------------------------------------------}

{-  
    Statement = Return | Block | While | LocalVarDecl | If | StmtOrExpr
-}

parseStmt :: EParser PositionedToken Stmt
parseStmt =     parseReturn
             <|> parseBlock
             <|> parseWhile
 --          <|> parseForLoop
             <|> parseLocalVarDecl
             <|> parseIf
             <|> parseStmtOrExprAsStmt
             <|> parseEmptyStmt

{-
parseSingletonStmt :: EParser PositionedToken Stmt
parseSingletonStmt =     parseLocalVarDecl
                      <|> (parseStmtOrExpr  >>> \(soe,pos) -> StmtOrExprAsStmt pos soe)
-}

{-
    Stmts = e | Stmt | Stmt : Stmts
-}
parseStmts :: EParser PositionedToken [Stmt]
parseStmts = manyE parseStmt

{-
    Block = '{' : Stmts : '}' 
          | '{' : '}' 
-}
parseBlock :: EParser PositionedToken Stmt
parseBlock =     ((item  LBRACKET +++ parseStmts +++ item  RBRACKET)  >>> (\(lb, (stmts, rb)) -> Block (makePos lb rb) stmts))
              <|> ((item  LBRACKET +++ item  RBRACKET)  >>> \(lb, rb) -> Block (makePos lb rb) [])

{-
    Return = 'return' : ';' | 'return' : Expr : ';'
-}
parseReturn :: EParser PositionedToken Stmt
parseReturn =     ((item  RETURN +++ item  SEMICOLON)
                     >>> (\(ret, semicolon) -> Return (makePos ret semicolon) Nothing))

               <|> ((item  RETURN +++ parseExpr +++ item  SEMICOLON)
                     >>> (\(ret, (expr, semicolon)) -> Return (makePos ret semicolon) (Just expr)))

{-
    While = 'while' : Expr : Stmt
-}
parseWhile :: EParser PositionedToken Stmt
parseWhile = (item  WHILE +++ parseExpr +++ parseStmt)
                 >>> (\(while, (expr, stmt))
                    -> While (spanPos (position while) (getPosFromStmt stmt)) expr stmt)
{-
parseForLoop :: EParser PositionedToken Stmt
parseForLoop = (item  FOR +++ item  LBRACE +++ parseSingletonStmt +++ item  SEMICOLON +++ parseExpr
                +++ item  SEMICOLON +++ parseSingletonStmt +++ item  RBRACE +++ parseStmt)
                     >>> \(for, (_, (stmt1, (_, (bexpr, (_, (stmt3, (_, block))))))))
                        ->  let pos = spanPos (position for) (getPosFromStmt block)
                                blockPos = getPosFromStmt block in
                                Block pos [stmt1, While pos bexpr (Block blockPos [block,stmt3])]
-}

{-
    LocalVarDecl = Type : LocalName : ';' 
                 | Type : LocalName : '=' : Expr : ';'
-}

-- note: parameter of (INTLITERAL __) is ignored
parseLocalVarDecl :: EParser PositionedToken Stmt
parseLocalVarDecl =     ((parseType +++ parseLocalName +++ item  SEMICOLON)
                             >>> (\((tpe, pos1), ((name, _), semicolon))
                                -> LocalVarDecl (spanPos pos1 (position semicolon)) tpe name Nothing))

                     <|> ((parseType +++ parseLocalName +++ item  ASSIGN +++ parseExpr +++ item  SEMICOLON)
                             >>> (\((tpe, pos1), ((name, _), (_, (expr, semicolon))))
                                -> LocalVarDecl (spanPos pos1 (position semicolon)) tpe name (Just expr)))

{-
    If = 'if' : Expr : Stmt 
       | 'if' : Expr : Stmt : 'else' : Stmt
-}
parseIf :: EParser PositionedToken Stmt
parseIf =     ((item  IF +++ parseExpr +++ parseStmt)
                 >>> (\(ifname, (expr, stmt))
                    -> If (spanPos (position ifname) (getPosFromStmt stmt)) expr stmt Nothing))

           <|> ((item  IF +++ parseExpr +++ parseStmt +++ item  ELSE +++ parseStmt)
                 >>> (\(ifname, (expr, (stmt1, (_, stmt2))))
                    -> If (spanPos (position ifname) (getPosFromStmt stmt2)) expr stmt1 (Just stmt2)))

{-
    StmtOrExprAsStmt = StmtOrExpr : ';'
-}
parseStmtOrExprAsStmt :: EParser PositionedToken Stmt
parseStmtOrExprAsStmt = (parseStmtOrExpr +++ item  SEMICOLON)  >>> (\((stOrEx, pos1), semicolon) -> StmtOrExprAsStmt (spanPos pos1 (position semicolon)) stOrEx)
{-
parseThisOrSuperCall :: EParser PositionedToken Stmt 
parseThisOrSuperCall =     ((item  THIS +++ item  LBRACE +++ parseCallParams +++ item  RBRACE) 
                             >>> \(this, (_, (params, rb))) -> ThisCall params)

                        <|> ((item  SUPER +++ item  LBRACE +++ parseCallParams +++ item  RBRACE)
                             >>> \(super, (_, (params, rb))) -> SuperCall params)
-}

parseEmptyStmt :: EParser PositionedToken Stmt 
parseEmptyStmt = item  SEMICOLON  >>> \semicolon -> Block (position semicolon) []

{-----------------------------------------------------------------------------}
{-- # StmtOrExpr -------------------------------------------------------------}
{-----------------------------------------------------------------------------}


parseStmtOrExpr :: EParser PositionedToken (StmtOrExpr, Position)

parseStmtOrExpr =     parseAssignment
                   <|> parseNew
                   <|> parseMethodCall

--left of dot only name as expression allowed
parseAssignment :: EParser PositionedToken (StmtOrExpr, Position)
parseAssignment =     ((parseLocalName +++ item  ASSIGN +++ parseExpr)
                         >>> (\((varName, pos1), (_, expr)) -> (Assign Nothing varName expr, spanPos pos1 (getPosFromExpr expr))))

                   <|> ((parseExpr +++ item  DOT +++ parseFieldName +++ item  ASSIGN +++ parseExpr)
                         >>> (\(expr1, (_, ((fn, _), (_, expr2)))) -> (Assign (Just expr1) fn expr2, spanPos (getPosFromExpr expr1) (getPosFromExpr expr2))))

parseNew :: EParser PositionedToken (StmtOrExpr, Position)
parseNew = (item  NEW +++ parseClassName +++ item  LBRACE +++ parseCallParams +++ item  RBRACE)
             >>> (\(new, ((name, _), (_, (callParams, rb)))) -> (New name callParams, makePos new rb))


parseMethodCall :: EParser PositionedToken (StmtOrExpr, Position)
parseMethodCall = ((parseMethodName +++ item  LBRACE +++ parseCallParams +++ item  RBRACE)
                     >>> (\((name, pos1), (_, (callParams, rb)))
                        -> (MethodCall Nothing name callParams, spanPos pos1 (position rb))))

                 <|> ((parseExpr +++ item  DOT +++ parseMethodName +++ item  LBRACE +++ parseCallParams +++ item  RBRACE)
                     >>> (\(expr, (_, ((mthName,_), (_, (callParams, rb)))))
                        -> (MethodCall (Just expr) mthName callParams, spanPos (getPosFromExpr expr) (position rb))))


parseCallParams :: EParser PositionedToken [Expr]
parseCallParams =      succeedE []
                   <|> (parseExpr  >>> (: []))
                   <|> ((parseExpr +++ item  COMMA +++ parseCallParams )
                         >>> (\(expr, (_, exprs)) -> expr : exprs))

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
parseTExpr :: EParser PositionedToken Expr
parseTExpr =    parseThis
             <|> parseSuper
             <|> parseName
             <|> parseUnary
             <|> parseLiteralExpr
             <|> parseStmtOrExprEasy
             <|> parseBraceExpr

-- general expressions
parseExpr :: EParser PositionedToken Expr
parseExpr =     ((parseTExpr +++ parseExpr')  >>> uncurry resolveRightSideExpr)
              <|> parseTExpr

-- right side expressions
parseExpr' :: EParser PositionedToken RightSideExpr
parseExpr' =     ((parseBinOp +++ parseExpr)                                                                         -- + expr
                     >>> uncurry RSbExpr)
              <|> ((item  DOT +++ parseFieldName)                                                                  -- .field Access
                     >>> \(dot, (fldName, pos)) -> RSfaExpr (spanPos (position dot) pos) fldName)
              <|> ((item  DOT +++ parseMethodName +++ item  LBRACE +++ parseCallParams +++ item  RBRACE)     -- .method call
                     >>> (\(dot, ((mthName, _), (_, (callParams, rb)))) -> RSmc (makePos dot rb) mthName callParams))
              <|> ((item  DOT +++ parseFieldName +++ item  ASSIGN +++ parseExpr)                                -- .assignment
                     >>> \(dot, ((fldName, _) , (_, expr))) -> RSassign (spanPos (position dot) (getPosFromExpr expr)) fldName expr)
              <|> ((item  DOT +++ parseFieldName +++ parseExpr')                                                   -- special case: chain .a.b.c... | .a + b
                     >>> \(dot, ((fldName, pos), texpr)) -> Chain (RSfaExpr (spanPos (position dot) pos) fldName) texpr)

parseThis :: EParser PositionedToken Expr
parseThis = item  THIS  >>> \tkn ->  This (position tkn)

parseSuper :: EParser PositionedToken Expr
parseSuper = item  SUPER  >>> \tkn -> Super (position tkn)

parseName :: EParser PositionedToken Expr
parseName =  parseLocalOrFieldOrClassName  >>> \(name, pos) -> Name pos name

parseUnary :: EParser PositionedToken Expr
parseUnary = (parseUnOp +++ parseExpr)  >>> \(op, expr) -> Unary (getPosFromExpr expr) op expr

parseLiteralExpr :: EParser PositionedToken Expr
parseLiteralExpr = parseLiteral  >>> \(lit, pos) -> Literal pos lit

parseBraceExpr :: EParser PositionedToken Expr
parseBraceExpr = (item  LBRACE +++ parseExpr +++ item  RBRACE)  >>> \(_, (expr, _)) -> expr

--parse stmt or expr without left recursion
parseStmtOrExprEasy :: EParser PositionedToken Expr
parseStmtOrExprEasy = ((parseMethodName +++ item  LBRACE +++ parseCallParams +++ item  RBRACE) -- parse method call without expression
                         >>> (\((mthName, pos1),(_, (callParams,rb)))
                            -> StmtOrExprAsExpr (spanPos pos1 (position rb)) (MethodCall Nothing mthName callParams)))

                     <|> (parseNew  >>> \(new, pos) -> StmtOrExprAsExpr pos new) -- parse 'new' expression

                     <|> ((parseLocalName +++ item  ASSIGN +++ parseExpr)
                         >>> (\((varName, pos1), (_, expr))
                            -> StmtOrExprAsExpr (spanPos pos1 (getPosFromExpr expr)) (Assign Nothing varName expr))) --parse simple assignment

parseUnOp :: EParser PositionedToken UnOparator -- still typo
parseUnOp =      (item  PLUS           >>> const Plus)
             <|> (item  MINUS          >>> const Minus)
             <|> (item  EXCLMARK       >>> const LNot)

parseBinOp :: EParser PositionedToken BinOperator
parseBinOp =      (item  PLUS          >>> const Add)
              <|> (item  MINUS         >>> const Sub)
              <|> (item  MUL           >>> const Mul)
              <|> (item  DIV           >>> const Div)
              <|> (item  MOD           >>> const Mod)
              <|> (item  AND           >>> const LAnd)
              <|> (item  OR            >>> const LOr)
              <|> (item  LESS          >>> const Types.Core.LT)
              <|> (item  LESSEQUAL     >>> const LTE)
              <|> (item  GREATER       >>> const Types.Core.GT)
              <|> (item  GREATEREQUAL  >>> const GTE)
              <|> (item  EQUAL         >>> const Types.Core.EQ)
              <|> (item  NOTEQUAL      >>> const NEQ)

-- placeholders for irrelevant arguments
intLit :: Integer
intLit = 0

charLit :: Char
charLit = 'a'

boolLit :: Bool
boolLit = True

anyString :: String
anyString = "anything"

-- all non exhaustive patterns are safe
parseLiteral :: EParser PositionedToken (Literal, Position)
parseLiteral =      (item (INTLITERAL intLit)
                         >>> (\PositionedToken { token = (INTLITERAL x), position = pos} -> (IntLit x, pos)))

                 <|> (item (CHARLITERAL charLit)
                         >>> (\PositionedToken { token = (CHARLITERAL chr), position = pos} -> (CharLit chr, pos)))

                 <|> (item (BOOLLITERAL boolLit)
                         >>> (\PositionedToken {token = (BOOLLITERAL bol), position = pos} -> (BoolLit bol, pos)))

                 <|> ( item JNULL
                         >>> (\tkn -> (Null, position tkn)))

{-----------------------------------------------------------------------------}
{-- # Types ------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- parameter of type (IDENTIFIER __) are ignored
parseType :: EParser PositionedToken (Type, Position)
parseType =     ( item CHAR >>> (\tkn -> (Char, position tkn)))
             <|> ( item INT >>> (\tkn -> (Int, position tkn)))
             <|> ( item BOOLEAN >>> (\tkn -> (Bool, position tkn)))
             <|> ( item VOID >>> (\tkn -> (Void, position tkn)))
             <|> (item (IDENTIFIER anyString)
                    >>> \PositionedToken { token = (IDENTIFIER name), position = pos} -> (Types.Core.Class name, pos)) -- this pattern is safe
             <|> (( item STRING +++  item LSQRBRACKET +++  item RSQRBRACKET)
                    >>> \(str, (_, rb)) -> (StringArr, makePos str rb))


{-----------------------------------------------------------------------------}
{-- # Visibilities -----------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseVisibility :: EParser PositionedToken AccessModifier
parseVisibility =     ( item PUBLIC >>> const Public)
                  <|> ( item PRIVATE >>> const Private)
                  <|> ( item PROTECTED >>> const Package)
                  <|> succeedE Package    -- default: package


{-----------------------------------------------------------------------------}
{-- # Identifier names -------------------------------------------------------}
{-----------------------------------------------------------------------------}

parseIdentifier :: EParser PositionedToken (Identifier, Position)
parseIdentifier = item (IDENTIFIER anyString)
                    >>> \PositionedToken {token = (IDENTIFIER name), position = pos} -> (name, pos) -- this pattern is safe

parseClassName :: EParser PositionedToken (ClassName, Position)
parseClassName = parseIdentifier

parseFieldName :: EParser PositionedToken (FieldName, Position)
parseFieldName = parseIdentifier

parseMethodName :: EParser PositionedToken (MethodName, Position)
parseMethodName = parseIdentifier

parseLocalName :: EParser PositionedToken (LocalName, Position)
parseLocalName = parseIdentifier

parseLocalOrFieldOrClassName :: EParser PositionedToken (LocalOrFieldOrClassName, Position)
parseLocalOrFieldOrClassName = parseIdentifier