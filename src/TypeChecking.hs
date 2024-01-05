module TypeChecking where

  import Control.Monad.State  hiding (void)
  import Data.List
  import Data.List.Extra
  import Debug
  import Debug.Trace
  import Text.Printf

  import AST
  import Environment
  import Type

  data TypeCheckedState         = TypeCheckedState { typeEnv :: TypeEnvironment, returnType :: Maybe Type }
  type TypeChecked t            = State TypeCheckedState t
  type TypeResult               = TypeChecked Type

  type UntypedBody              = Body ()
  type UntypedExp               = Expression ()
  type UntypedExps              = Expressions ()
  type UntypedLExp              = LeftExpression ()
  type UntypedPexp              = PointerExpression ()
  type UntypedStatement         = Statement ()
  type UntypedStatements        = Statements ()
  type UntypedDeclaration       = Declaration ()
  type UntypedDeclarations      = Declarations ()

  type TypedBody                = TypeChecked (Type, Body Type)
  type TypedExp                 = TypeChecked (Type, Expression Type)
  type TypedExps                = TypeChecked (Expressions Type)
  type TypedLExp                = TypeChecked (Type, LeftExpression Type)
  type TypedPexp                = TypeChecked (Type, PointerExpression Type)
  type TypedStatement           = TypeChecked (Type, Statement Type)
  type TypedStatements          = TypeChecked (Type, Statements Type)
  type TypedDeclaration         = TypeChecked (Declaration Type)
  type TypedDeclarations        = TypeChecked (Declarations Type)

  unknownVariableRefError :: Name -> TypeChecked a
  unknownVariableRefError var = error $ printf "Unknown variable %s" var

  initState :: TypeCheckedState
  initState =
    let emptyEnv = Environment.empty
        initEnv = Environment.insert "print" (ArrowType [Atom IntType] $ Atom VoidType) emptyEnv
    in TypeCheckedState initEnv Nothing

  isIntegralType :: Type -> TypeChecked Bool
  isIntegralType (Atom IntType) = return True
  isIntegralType (Atom CharType) = return True
  isIntegralType _ = return False

  getAtomicTypeType :: Type -> TypeChecked AtomicType
  getAtomicTypeType (Atom CharType) = return CharType
  getAtomicTypeType (Atom IntType) = return IntType
  getAtomicTypeType _ = fail "Not an atomic type"

  isArrayType :: Type -> Bool
  isArrayType (ArrayType _ _) = True
  isArrayType t = False

  isPointerType :: Type -> Bool
  isPointerType (ArrayType _ _) = True
  isPointerType (PointerType _) = True
  isPointerType _ = False

  isPointerTypePointingTo :: Type -> Type -> Bool
  isPointerTypePointingTo (ArrayType _ t) pointingTo = t == pointingTo
  isPointerTypePointingTo (PointerType t) pointingTo = t == pointingTo
  isPointerTypePointingTo _ _ = False

  equalsAtomicType :: AtomicType -> AtomicType -> Bool
  equalsAtomicType atomTyp1 atomTyp2 = (atomTyp1 == atomTyp2) ||
                                       case (atomTyp1, atomTyp2) of 
                                         (IntType, CharType) -> True
                                         (CharType, IntType) -> True
                                         (_, _) -> False

  equalsType :: Type -> Type -> Bool
  equalsType (Atom atomTyp1) (Atom atomTyp2) = equalsAtomicType atomTyp1 atomTyp2
  equalsType (ArrowType argTypes1 resultType1) (ArrowType argTypes2 resultType2) =
     (length argTypes1 == length argTypes2) &&
     (equalsType resultType1 resultType2) &&
     let zipped = zip argTypes1 argTypes2
         f = \(a, b) -> equalsType a b
     in all f zipped
  equalsType (ArrayType size1 pointedTo) other = isPointerTypePointingTo other pointedTo
  equalsType (PointerType pointedTo) other = isPointerTypePointingTo other pointedTo
  equalsType _ _ = False

  typeOfArrayType :: Type -> TypeChecked Type
  typeOfArrayType (ArrayType _ arrayElementType) = return arrayElementType
  typeOfArrayType t = error $ printf "Type %s is not an array type" $ show t

  typeOfArrayRefExp :: UntypedLExp -> UntypedExp -> TypedLExp
  typeOfArrayRefExp arrayExp indexExp =
    do (arrayType, tArrayExp) <- typeCheckLExp arrayExp
       arrayRefType <- typeOfArrayType arrayType
       (indexType, tIndexExp) <- typeCheckExp indexExp
       -- TODO : check indexType is an integral type
       let tArrayRefExp = ArrayRefExp tArrayExp tIndexExp arrayRefType
       return (arrayRefType, tArrayRefExp)

  typeOfIntegralUnOp :: UnOperator -> UntypedExp -> TypedExp
  typeOfIntegralUnOp unOp exp =
    do (expType, tExp) <- typeCheckExp exp
       cond <- isIntegralType expType
       let unOpType = expType
       if cond
       then return $ (unOpType, UnaryExp unOp tExp unOpType)
       else error $ printf "%s expected integral type, received a %s instead" (show unOp) $ show tExp

  typeOfIntegralBinOp :: BinOperator -> UntypedExp -> UntypedExp -> (Type -> TypeChecked Bool) -> TypedExp
  typeOfIntegralBinOp binOp exp1 exp2 pred =
    do (exp1Type, tExp1) <- typeCheckExp exp1
       (exp2Type, tExp2) <- typeCheckExp exp2
       cond1 <- isIntegralType exp1Type
       cond2 <- isIntegralType exp2Type
       if cond1 && cond2
       then do atom1Type <- getAtomicTypeType exp1Type
               atom2Type <- getAtomicTypeType exp2Type
               let binOpType = Atom $ castAtomicTypes atom1Type atom2Type
               return (binOpType, BinaryExp binOp tExp1 tExp2 binOpType)
       else error $ printf "%s expected integral types, received a %s and a %s instead" (show binOp) (show tExp1) $ show tExp2

  typeOfBinaryIntegralOrPointerExp :: UntypedExp -> UntypedExp -> BinOperator -> TypedExp
  typeOfBinaryIntegralOrPointerExp exp1 exp2 binOp =
    do (exp1Type, tExp1) <- typeCheckExp exp1
       (exp2Type, tExp2) <- typeCheckExp exp2
       exp1IsIntegral <- isIntegralType exp1Type
       let exp1IsPointer = isPointerType exp1Type
       let cond1 = exp1IsIntegral || exp1IsPointer
       cond2 <- isIntegralType exp2Type
       if cond1 && cond2
       then return (exp1Type, BinaryExp binOp tExp1 tExp2 exp1Type)
       else error $ printf "%s expected integral or pointer types, received a %s and a %s instead" (show binOp) (show tExp1) $ show tExp2

  typeOfBinaryExp :: UntypedExp -> UntypedExp -> BinOperator -> TypedExp
  typeOfBinaryExp exp1 exp2 PlusOp = typeOfBinaryIntegralOrPointerExp exp1 exp2 PlusOp
  typeOfBinaryExp exp1 exp2 MinusOp = typeOfBinaryIntegralOrPointerExp exp1 exp2 MinusOp
  typeOfBinaryExp exp1 exp2 binOp = typeOfIntegralBinOp binOp exp1 exp2 isIntegralType

  expsMatchTypes :: UntypedExps -> [Type] -> TypeChecked [(Expression Type, Type)]
  expsMatchTypes exps types =
    Prelude.foldl (\prev curr -> let (exp, expectedType) = curr
                                 in do tExps <- prev
                                       (expType, tExp) <- typeCheckExp exp
                                       if equalsType expType expectedType
                                       then return $ tExps `snoc` (tExp, expectedType)
                                       else fail $ printf "%s was expected to be of type %s; is of type %s instead" (show exp) (show tExp) (show expectedType))
                  (return []) $
                  zip exps types

  typeOfFunctionAppExp :: Name -> UntypedExps -> TypedExp
  typeOfFunctionAppExp fName exps = 
    do state <- get
       let env = typeEnv state
       case Environment.lookup fName env of
        Nothing -> unknownVariableRefError fName
        Just (ArrowType tArgs tResult) -> do tExps <- expsMatchTypes exps tArgs
                                             let tExp = FunctionAppWithTypedExp fName tExps tResult
                                             return (tResult, tExp)
        other -> error $ printf "Operator in function application has incorrect type: %s" $ show other

  typeOfLengthExp :: UntypedExp -> TypedExp
  typeOfLengthExp exp =
    do (expType, tExp) <- typeCheckExp exp
       if isArrayType expType
       then let typ = Atom IntType in return (typ, LengthExp tExp typ)
       else error $ printf "Length expected an array, received a %s" $ show tExp
    
  typeOfUnaryExp :: UntypedExp -> UnOperator -> TypedExp
  typeOfUnaryExp exp unOp = typeOfIntegralUnOp unOp exp

  typeOfUnaryModifyingExp :: UntypedLExp -> UnModifyingOperator -> TypedExp
  typeOfUnaryModifyingExp lexp unOp =
    do (expType, tLexp) <- typeCheckLExp lexp
       cond <- isIntegralType expType
       let unOpType = expType
       if cond
       then return $ (unOpType, UnaryModifyingExp unOp tLexp unOpType)
       else error $ printf "%s expected integral type, received a %s instead" (show unOp) $ show tLexp

  typeOfVariableRefExp :: Name -> TypedLExp
  typeOfVariableRefExp var = 
    do state <- get
       let env = typeEnv state
       case Environment.lookup var env of
        Just t -> return (t, VariableRefExp var t)
        Nothing -> unknownVariableRefError var

  typeCheckPExp :: UntypedPexp -> TypedPexp
  typeCheckPExp (PointerExp lexp exp _) =
    do (lexpType, tLexp) <- typeCheckLExp lexp
       (expType, tExp) <- typeCheckExp exp
       cond <- isIntegralType expType
       if (not cond)
       then error $ printf "Expression %s should be an integral type" (show tExp)
       else case lexpType of
          PointerType pointedTo -> return (PointerType pointedTo, PointerExp tLexp tExp $ PointerType pointedTo)
          ArrayType _ pointedTo -> return (PointerType pointedTo, PointerExp tLexp tExp $ PointerType pointedTo)
          _ -> error $ printf "Expression %s is not a pointer type" (show tLexp)

  typeCheckLExp :: UntypedLExp -> TypedLExp
  typeCheckLExp (ArrayRefExp array index _) = typeOfArrayRefExp array index
  typeCheckLExp (VariableRefExp var _) = typeOfVariableRefExp var
  typeCheckLExp (DerefExp pexp _) =
    do (pexpType, tPexp) <- typeCheckPExp pexp
       case pexpType of
          PointerType pointedTo -> return (pointedTo, DerefExp tPexp pointedTo)
          ArrayType _ pointedTo -> return (pointedTo, DerefExp tPexp pointedTo)
          _ -> error $ printf "Expression %s is not a pointer type" (show tPexp)

  typeCheckExp :: UntypedExp -> TypedExp
  typeCheckExp exp = case exp of
      NumberExp n _ -> let typ = Atom lowestAtomicType in return (typ, NumberExp n typ)
      QCharExp c _ -> let typ = Atom lowestAtomicType in return (typ, QCharExp c typ)
      BinaryExp binOp exp1 exp2 _ -> typeOfBinaryExp exp1 exp2 binOp
      FunctionAppExp name exps _ -> typeOfFunctionAppExp name exps
      LengthExp exp _ -> typeOfLengthExp exp
      UnaryExp unOp exp _ -> typeOfUnaryExp exp unOp
      UnaryModifyingExp unOp lexp _ -> typeOfUnaryModifyingExp lexp unOp
      AddressOf lexp _ -> do (lexpType, tLexp) <- typeCheckLExp lexp
                             let expType = PointerType lexpType
                             return (expType, AddressOf tLexp expType)
      LeftExp lexp _ -> do (lexpType, tLexp) <- typeCheckLExp lexp
                           return (lexpType, LeftExp tLexp lexpType)

  typeCheckAndCastExp :: UntypedExp -> Type -> TypedExp
  typeCheckAndCastExp exp typ =
    do (expType, tExp) <- typeCheckExp exp
       if (equalsType expType typ)
       then return (typ, setExpT tExp typ)
       else error $ printf "Cannot cast exp %s to typ %s" (show tExp) (show typ)

  void :: Type
  void = Atom VoidType

  typeCheckStatement :: UntypedStatement -> TypedStatement
  typeCheckStatement (AssignStmt lexp exp _) = case lexp of
      VariableRefExp var _ -> do (varType, tVar) <- typeOfVariableRefExp var
                                 (expType, tExp) <- typeCheckExp exp
                                 let tLexp = VariableRefExp var varType
                                 if (equalsType expType varType)
                                 then return (void, AssignStmt tLexp tExp void)
                                 else error $ printf "Assigning incorrect type %s to variable %s (of type %s)" (show tExp) var $ show varType
      ArrayRefExp array idx _ -> do (idxType, tIdx) <- typeCheckExp idx
                                    (expType, tExp) <- typeCheckExp exp
                                    (arrayType, tArrayExp) <- typeCheckLExp array
                                    isIntegral <- isIntegralType idxType
                                    if (not isIntegral)
                                    then error $ printf "Incorrect type for the index-expression: expected Integral type, but is %s" $ show idxType
                                    else case arrayType of
                                      ArrayType _ arrayExpType ->
                                        if (arrayExpType /= expType)
                                           then error $ printf "Assigning incorrect type %s to array-element of type %s" (show tExp) $ show tArrayExp
                                           else return (void, AssignStmt (ArrayRefExp tArrayExp tIdx arrayExpType) tExp void)
                                      _ -> error $ printf "Not an array-type: %s" $ show arrayType
      DerefExp pexp _ -> do (pexpType, tPexp) <- typeCheckPExp pexp
                            (expType, tExp) <- typeCheckExp exp
                            case pexpType of
                              PointerType pointedTo -> return (void, AssignStmt (DerefExp tPexp pexpType) tExp void)
                              ArrayType _ pointedTo -> return (void, AssignStmt (DerefExp tPexp pexpType) tExp void)
                              _ -> error $ printf "Expression %s is not a pointer type" (show tPexp)
  -- typeCheckStatement (AssignOpStmt op lexp exp t) = 
  --   let newRExp = BinaryExp (assignableBinOpToBinOp op) (LeftExp lexp t) exp t
  --   in typeCheckStatement $ AssignStmt lexp newRExp t
  typeCheckStatement (BlockStmt block _) =
    do (blockType, tBlock) <- typeCheckBlock block
       return (void, BlockStmt tBlock void)
  typeCheckStatement (ExpStmt exp _) =
    do (expType, tExp) <- typeCheckExp exp
       return (void, ExpStmt tExp void)
  typeCheckStatement (ForStmt init m1 pred m2 inc m3 body m4 _) =
    do (_, tInit) <- typeCheckStatement init
       (predType, tPred) <- typeCheckExp pred
       isIntegral <- isIntegralType predType
       if isIntegral
       then return ()
       else error $ printf "Expression %s is not an integral type" (show pred)
       (_, tInc) <- typeCheckStatement inc
       (_, tBody) <- typeCheckBlock body
       return (void, ForStmt tInit m1 tPred m2 tInc m3 tBody m4 void)
  typeCheckStatement (IfElseStmt pred thenBody m1 elseBody m2 _) =
    -- Type of the predicate can be everything: is not restricted to an integral type
    do (predType, tPred) <- typeCheckExp pred
       (_, tThenBody) <- typeCheckBlock thenBody
       (_, tElseBody) <- typeCheckBlock elseBody
       return $ (void, IfElseStmt tPred tThenBody m1 tElseBody m2 void)
  typeCheckStatement (IfStmt pred thenBody m1 _) =
    -- Type of the predicate can be everything: is not restricted to an integral type
    do (predType, tPred) <- typeCheckExp pred
       (_, tThenBody) <- typeCheckBlock thenBody
       return $ (void, IfStmt tPred tThenBody m1 void)
  typeCheckStatement (Return0Stmt _) =
    -- should check whether function's return-type is void
    return (void, Return0Stmt void)
  typeCheckStatement (Return1Stmt exp _) =
    do state <- get
       (expType, tExp) <- typeCheckExp exp
       let maybeReturnType = returnType state
       case maybeReturnType of
         Nothing -> return (void, Return1Stmt tExp void)
         Just returnType ->
           if (equalsType expType returnType)
           then return (void, Return1Stmt tExp void)
           else error $ printf "Returning '%s' from a function with incompatible result type '%s'"
                               (show tExp)
                               (show returnType)
  typeCheckStatement (ReadStmt lexp _) =
    do (lexpType, tLexp) <- typeCheckLExp lexp
       return (void, ReadStmt tLexp void)
  typeCheckStatement (WriteStmt lexp _) =
    do (lexpType, tLexp) <- typeCheckLExp lexp
       return (void, WriteStmt tLexp void)
  typeCheckStatement (WhileStmt m1 pred body m2 _) =
    -- Should check that predType is an integral type?
    do (predType, tPred) <- typeCheckExp pred
       (_, tBody) <- typeCheckBlock body
       return (void, WhileStmt m1 tPred tBody m2 void)

  typeCheckStatements' :: UntypedStatements -> (Statements Type) -> TypedStatements
  typeCheckStatements' [] acc = return (void, acc)
  typeCheckStatements' (stmt:stmts) acc =
    do (_, tStmt) <- typeCheckStatement stmt
       typeCheckStatements' stmts $ acc `snoc` tStmt

  typeCheckStatements :: UntypedStatements -> TypedStatements
  typeCheckStatements stmts = typeCheckStatements' stmts []

  declarationsToTypes :: UntypedDeclarations -> [Type]
  declarationsToTypes [] = []
  declarationsToTypes ((VarDeclaration typ _ _):decls) = typ : declarationsToTypes decls
  declarationsToTypes ((FunDeclaration retTyp _ parDecls _ _):decls) = (ArrowType (declarationsToTypes parDecls) retTyp) : declarationsToTypes decls

  typeCheckDeclaration :: UntypedDeclaration -> TypedDeclaration
  typeCheckDeclaration (VarDeclaration typ name _) =
    do state <- get
       let env = typeEnv state
       let newEnv = Environment.insert name typ env
       let state' = state{typeEnv = newEnv}
       put state'
       return $ VarDeclaration typ name void
  typeCheckDeclaration (FunDeclaration typ name decls body _) =
    do state <- get
       let env = typeEnv state
       -- Push new frame for the function's parameters
       let frameAddedEnv = Environment.push env
            -- Add parameters to the environment
       put state{typeEnv = frameAddedEnv, returnType = Just typ}
       tDecls <- typeCheckDeclarations' decls []
       -- Typecheck the function's body
       (_, tBody) <- typeCheckBlock body
       -- Insert the function declaration into the _original_ environment
       let newEnv = Environment.insert name (ArrowType (declarationsToTypes decls) typ) env
       put state{typeEnv = newEnv}
       return $ FunDeclaration typ name tDecls tBody void

  typeCheckDeclarations' :: UntypedDeclarations -> (Declarations Type) -> TypedDeclarations
  typeCheckDeclarations' [] acc = return acc
  typeCheckDeclarations' (decl:decls) acc =
    do tDecl <- typeCheckDeclaration decl
       typeCheckDeclarations' decls $ acc `snoc` tDecl

  typeCheckDeclarations :: UntypedDeclarations -> TypedDeclarations
  typeCheckDeclarations decls = typeCheckDeclarations' decls []

  typeCheckBlock' :: UntypedBody -> Body Type -> TypedBody
  typeCheckBlock' (Body [] [] _) accBody = return (void, accBody)
  typeCheckBlock' (Body decls stmts _) (Body tDecls tSmts _)  =
    do state <- get
       let env = typeEnv state
       tDecls <- typeCheckDeclarations' decls []
       (_, tStmts) <- typeCheckStatements stmts
       put state{typeEnv = env}
       return (void, Body tDecls tStmts void)

  typeCheckBlock :: UntypedBody -> TypedBody
  typeCheckBlock body =
    do state <- get
       let env = typeEnv state
       let frameAddedEnv = Environment.push env
       let emptyTypedBody = Body [] [] void
       typeCheckBlock' body emptyTypedBody

  typeCheck :: UntypedDeclarations -> Declarations Type
  typeCheck decls = fst $ runState (typeCheckDeclarations decls) initState
