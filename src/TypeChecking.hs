module TypeChecking where

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

    typeOfArrayRefExp :: Environment -> Expression -> Expression -> TypeResult
    typeOfArrayRefExp env arrayExp indexExp = do arrayType <- typeOfExp env arrayExp
                                                 typeOfArrayType arrayType

    typeOfIntegralUnOp :: Environment -> UnOperator -> Expression -> (Type -> TypeChecked Bool) -> TypeResult
    typeOfIntegralUnOp env unOp exp pred = do tExp <- typeOfExp env exp
                                              cond <- isIntegralType tExp
                                              if cond
                                              then return $ Atom IntType
                                              else error $ printf "%s expected integral type, received a %s instead" (show unOp) $ show tExp

    typeOfIntegralBinOp :: Environment -> BinOperator -> Expression -> Expression -> (Type -> TypeChecked Bool) -> TypeResult
    typeOfIntegralBinOp env binOp exp1 exp2 pred = do tExp1 <- typeOfExp env exp1
                                                      tExp2 <- typeOfExp env exp2
                                                      cond1 <- isIntegralType tExp1
                                                      cond2 <- isIntegralType tExp2
                                                      if cond1 && cond2
                                                      then return $ Atom IntType
                                                      else error $ printf "%s expected integral types, received a %s and a %s instead" (show binOp) (show tExp1) $ show tExp2

    typeOfBinaryExp :: Environment -> Expression -> Expression -> BinOperator -> TypeResult
    typeOfBinaryExp env exp1 exp2 binOp = typeOfIntegralBinOp env binOp exp1 exp2 isIntegralType

    expsMatchTypes :: Environment -> [Expression] -> [Type] -> TypeChecked ()
    expsMatchTypes env exps types = Prelude.foldl (\prev curr -> let (exp, expectedType) = curr in
                                                                 do cond <- prev
                                                                    tExp <- typeOfExp env exp
                                                                    if tExp == expectedType
                                                                    then return ()
                                                                    else fail $ printf "%s was expected to be of type %s; is of type %s instead" (show exp) (show tExp) (show expectedType))
                                          (return ()) $ zip exps types

    typeOfFunctionAppExp :: Environment -> Name -> [Expression] -> TypeResult
    typeOfFunctionAppExp env var exps = case Environment.lookup var env of
      Nothing -> unknownVariableRefError var
      Just (ArrowType tArgs tResult) -> expsMatchTypes env exps tArgs >> return tResult
      other -> error $ printf "Operator in function application has incorrect type: %s" $ show other

    typeOfLengthExp :: Environment -> Expression -> TypeResult
    typeOfLengthExp env exp = do tExp <- typeOfExp env exp
                                 if isArrayType tExp
                                 then return $ Atom IntType
                                 else error $ printf "Length expected an array, received a %s" $ show tExp
      
    typeOfUnaryExp :: Environment -> Expression -> UnOperator -> TypeResult
    typeOfUnaryExp env exp unOp = typeOfIntegralUnOp env unOp exp isIntegralType

    typeOfVariableRefExp :: Environment -> Name -> TypeResult
    typeOfVariableRefExp env var = case Environment.lookup var env of
      Just t -> return t
      Nothing -> unknownVariableRefError var

    typeOfExp :: Environment -> Expression -> TypeResult
    typeOfExp env exp = case exp of
        NumberExp _ -> return $ Atom IntType
        QCharExp _ -> return $ Atom CharType
        BinaryExp binOp exp1 exp2 -> typeOfBinaryExp env exp1 exp2 binOp
        FunctionAppExp name exps -> typeOfFunctionAppExp env name exps
        LengthExp exp -> typeOfLengthExp env exp
        UnaryExp unOp exp -> typeOfUnaryExp env exp unOp
        LeftExp (ArrayRefExp array index) -> typeOfArrayRefExp env array index
        LeftExp (VariableRefExp var) -> typeOfVariableRefExp env var

    typeCheckStatement :: Environment -> Statement -> TypeResult
    typeCheckStatement env stmt = return $ Atom IntType -- case stmt of
      -- AssignStmt lexp exp -> case lexp of
      --   VariableRefExp var -> return $ Atom IntType -- case Environment.lookup var of
          -- Just typ -> do tExp <- typeOfExp exp
          --                if (tExp == typ)
          --                then return typ
          --                else error $ printf "Assigning incorrect type %s to variable of type %s" $ show tExp $ show typ
          -- Nothing -> unknownVariableRefError var                


    --typeCheck :: Declarations -> TypeResult

