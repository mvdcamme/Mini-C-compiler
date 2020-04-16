module TypeChecking where

    import Debug
    import Text.Printf

    import AST
    import Environment
    import Type

    type TypeChecked t = Either String t
    type TypeResult = TypeChecked Type


    unknownVariableRefError :: Name -> TypeResult
    unknownVariableRefError var = error $ printf "Unknown variable %s" var

    isIntegralType :: Type -> TypeChecked Bool
    isIntegralType (Atom IntType) = return True
    isIntegralType (Atom CharType) = return True
    isIntegralType _ = return False

    isArrayType :: Type -> Bool
    isArrayType (ArrayType _ arrayElementType) = True
    isArrayType t = False

    equalsAtomicType :: AtomicType -> AtomicType -> Bool
    equalsAtomicType atomTyp1 atomTyp2 = case (atomTyp1, atomTyp2) of 
      (IntType, IntType) -> True
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
    equalsType (ArrayType size1 resultType1) (ArrayType size2 resultType2) = (size1 == size2) && (resultType1 == resultType2) --  TODO

    typeOfArrayType :: Type -> TypeResult
    typeOfArrayType (ArrayType _ arrayElementType) = return arrayElementType
    typeOfArrayType t = error $ printf "Type %s is not an array type" $ show t

    typeOfArrayRefExp :: TypeEnvironment -> Expression -> Expression -> TypeResult
    typeOfArrayRefExp env arrayExp indexExp = do arrayType <- typeCheckExp env arrayExp
                                                 typeOfArrayType arrayType

    typeOfIntegralUnOp :: TypeEnvironment -> UnOperator -> Expression -> (Type -> TypeChecked Bool) -> TypeResult
    typeOfIntegralUnOp env unOp exp pred = do tExp <- typeCheckExp env exp
                                              cond <- isIntegralType tExp
                                              if cond
                                              then return $ Atom IntType
                                              else error $ printf "%s expected integral type, received a %s instead" (show unOp) $ show tExp

    typeOfIntegralBinOp :: TypeEnvironment -> BinOperator -> Expression -> Expression -> (Type -> TypeChecked Bool) -> TypeResult
    typeOfIntegralBinOp env binOp exp1 exp2 pred = do tExp1 <- typeCheckExp env exp1
                                                      tExp2 <- typeCheckExp env exp2
                                                      cond1 <- isIntegralType tExp1
                                                      cond2 <- isIntegralType tExp2
                                                      if cond1 && cond2
                                                      then return $ Atom IntType
                                                      else error $ printf "%s expected integral types, received a %s and a %s instead" (show binOp) (show tExp1) $ show tExp2

    typeOfBinaryExp :: TypeEnvironment -> Expression -> Expression -> BinOperator -> TypeResult
    typeOfBinaryExp env exp1 exp2 binOp = typeOfIntegralBinOp env binOp exp1 exp2 isIntegralType

    expsMatchTypes :: TypeEnvironment -> [Expression] -> [Type] -> TypeChecked ()
    expsMatchTypes env exps types = Prelude.foldl (\prev curr -> let (exp, expectedType) = curr in
                                                                 do cond <- prev
                                                                    tExp <- typeCheckExp env exp
                                                                    if tExp == expectedType
                                                                    then return ()
                                                                    else fail $ printf "%s was expected to be of type %s; is of type %s instead" (show exp) (show tExp) (show expectedType))
                                          (return ()) $ zip exps types

    typeOfFunctionAppExp :: TypeEnvironment -> Name -> [Expression] -> TypeResult
    typeOfFunctionAppExp env var exps = case Environment.lookup var env of
      Nothing -> unknownVariableRefError var
      Just (ArrowType tArgs tResult) -> expsMatchTypes env exps tArgs >> return tResult
      other -> error $ printf "Operator in function application has incorrect type: %s" $ show other

    typeOfLengthExp :: TypeEnvironment -> Expression -> TypeResult
    typeOfLengthExp env exp = do tExp <- typeCheckExp env exp
                                 if isArrayType tExp
                                 then return $ Atom IntType
                                 else error $ printf "Length expected an array, received a %s" $ show tExp
      
    typeOfUnaryExp :: TypeEnvironment -> Expression -> UnOperator -> TypeResult
    typeOfUnaryExp env exp unOp = typeOfIntegralUnOp env unOp exp isIntegralType

    typeOfVariableRefExp :: TypeEnvironment -> Name -> TypeResult
    typeOfVariableRefExp env var = case Environment.lookup var env of
      Just t -> return t
      Nothing -> unknownVariableRefError var

    typeCheckLExp :: TypeEnvironment -> LeftExpression -> TypeResult
    typeCheckLExp env (ArrayRefExp array index) = typeOfArrayRefExp env array index
    typeCheckLExp env (VariableRefExp var) = typeOfVariableRefExp env var

    typeCheckExp :: TypeEnvironment -> Expression -> TypeResult
    typeCheckExp env exp = case exp of
        NumberExp _ -> return $ Atom IntType
        QCharExp _ -> return $ Atom CharType
        BinaryExp binOp exp1 exp2 -> typeOfBinaryExp env exp1 exp2 binOp
        FunctionAppExp name exps -> typeOfFunctionAppExp env name exps
        LengthExp exp -> typeOfLengthExp env exp
        UnaryExp unOp exp -> typeOfUnaryExp env exp unOp
        LeftExp lexp -> typeCheckLExp env lexp

    typeCheckStatement :: TypeEnvironment -> Statement -> TypeChecked ()
    typeCheckStatement env (AssignStmt lexp exp) = case lexp of
        VariableRefExp var -> do typ <- typeOfVariableRefExp env var
                                 tExp <- typeCheckExp env exp
                                 if (tExp == typ)
                                 then return ()
                                 else error $ printf "Assigning incorrect type %s to variable %s (of type %s)" (show tExp) var $ show typ
        ArrayRefExp array idx -> do idxType <- typeCheckExp env idx
                                    tExp <- typeCheckExp env exp
                                    arrayExp <- typeCheckExp env array
                                    if (idxType /= Atom IntType)
                                    then error $ printf "Incorrect type for the index-expression: expected Integral type, but is %s" $ show idxType
                                    else if (arrayExp /= tExp)
                                         then error $ printf "Assigning incorrect type %s to array-element of type %s" (show tExp) $ show arrayExp
                                         else return ()
    typeCheckStatement env (BlockStmt block) = typeCheckBlock env block >> return ()
    typeCheckStatement env (ExpStmt exp) = typeCheckExp env exp >> return ()
    typeCheckStatement env (IfStmt pred thenBody _ elseBody _) = do typeCheckExp env pred -- Type of the predicate can be everything: is not restricted to an integral type
                                                                    typeCheckBlock env thenBody
                                                                    typeCheckBlock env elseBody
                                                                    return ()
    typeCheckStatement env (Return1Stmt exp) = typeCheckExp env exp >> return ()
    typeCheckStatement env (ReadStmt lexp) = typeCheckLExp env lexp >> return ()
    typeCheckStatement env (WriteStmt lexp) = typeCheckLExp env lexp >> return ()
    typeCheckStatement env (WhileStmt _ pred body _) = typeCheckExp env pred >> typeCheckBlock env body >> return ()

    typeCheckStatements :: TypeEnvironment -> [Statement] -> TypeChecked ()
    typeCheckStatements _ [] = return ()
    typeCheckStatements env (stmt:stmts) = (typeCheckStatement env stmt) >> (typeCheckStatements env stmts)

    declarationsToTypes :: [Declaration] -> [Type]
    declarationsToTypes [] = []
    declarationsToTypes ((VarDeclaration typ _):decls) = typ : declarationsToTypes decls
    declarationsToTypes ((FunDeclaration retTyp _ parDecls _):decls) = (ArrowType (declarationsToTypes parDecls) retTyp) : declarationsToTypes decls

    typeCheckDeclaration :: TypeEnvironment -> Declaration -> TypeChecked TypeEnvironment
    typeCheckDeclaration env (VarDeclaration typ name) = return $ Environment.insert name typ env
    typeCheckDeclaration env (FunDeclaration typ name decls body) = let frameAddedEnv = Environment.push env -- Push new frame for the function's parameters
                                                                    in do updatedEnv <- typeCheckDeclarations' frameAddedEnv decls -- Add parameters to the environment
                                                                          typeCheckBlock updatedEnv body -- Typecheck the function's body
                                                                          return $ Environment.insert name (ArrowType (declarationsToTypes decls) typ) env -- Insert the function declaration into the _original_ environment

    typeCheckDeclarations' :: TypeEnvironment -> [Declaration] -> TypeChecked TypeEnvironment
    typeCheckDeclarations' env [] = return env
    typeCheckDeclarations' env (decl:decls) = typeCheckDeclaration env decl >>= \(updatedEnv) -> typeCheckDeclarations' updatedEnv decls

    typeCheckDeclarations :: [Declaration] -> TypeChecked TypeEnvironment
    typeCheckDeclarations decls = let emptyEnv = Environment.empty
                                      initEnv = Environment.insert "print" (ArrowType [Atom IntType] $ Atom VoidType) emptyEnv 
                                  in typeCheckDeclarations' initEnv decls

    typeCheckBlock' :: TypeEnvironment -> Body -> TypeChecked ()
    typeCheckBlock' _ (Body [] []) = return ()
    typeCheckBlock' env (Body decls stmts) = typeCheckDeclarations' env decls >>= \(updatedEnv) -> typeCheckStatements updatedEnv stmts

    typeCheckBlock :: TypeEnvironment -> Body -> TypeChecked ()
    typeCheckBlock env body = let frameAddedEnv = Environment.push env
                              in typeCheckBlock' frameAddedEnv body



    --typeCheck :: Declarations -> TypeResult

