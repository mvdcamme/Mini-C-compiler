module TypedAST where
  import AST
  import Type

  data TypedExpression            = TypedLeftExp LeftExpression
                                    | TypedBinaryExp BinOperator Expression Expression
                                    | TypedUnaryExp UnOperator Expression
                                    | TypedUnaryModifyingExp UnModifyingOperator LeftExpression
                                    | TypedFunctionAppExp Name Expressions
                                    | TypedLengthExp Expression
                                    | TypedAddressOf LeftExpression
                                    | TypedNumberExp Integer
                                    | TypedQCharExp Char
                                    deriving (Show, Eq)

  data TypedDeclaration           = TypedVarDeclaration Type Name
                                    | TypedFunDeclaration Type Name TypedDeclarations Body
                                    deriving (Show, Eq)
  type TypedDeclarations          = [TypedDeclaration]

  data TypedStatement             = 