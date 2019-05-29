module AST where

    import Type

    type Name =                     String

    data Declaration =              VarDeclaration Type Name
                                    | FunDeclaration Type Name Declarations Body
                                    deriving (Show, Eq)

    type Declarations =             [Declaration]

    data Statement =                AssignStmt LeftExpression Expression
                                    | BlockStmt Body
                                    | ExpStmt Expression 
                                    | IfStmt Expression Body Body
                                    | ReturnStmt Expression
                                    | ReadStmt LeftExpression
                                    | WriteStmt LeftExpression
                                    | WhileStmt Marker Expression Marker Body Marker
                                    deriving (Show, Eq)

    type Statements =               [Statement]

    data Body =                     Body Declarations Statements
                                    deriving (Show, Eq)

    data LeftExpression =           VariableRefExp Name
                                    | ArrayRefExp Expression Expression
                                    deriving (Show, Eq) 

    data Expression =               LeftExp LeftExpression
                                    | BinaryExp BinOperator Expression Expression
                                    | UnaryExp UnOperator Expression
                                    | FunctionAppExp Name Expressions
                                    | LengthExp Expression
                                    | NumberExp Integer
                                    | QCharExp Char
                                    deriving (Show, Eq)

    type Expressions =              [Expression]

    data BinOperator =              PlusOp
                                    | MinusOp
                                    | TimesOp
                                    | DivideOp
                                    | EqualOp
                                    | NequalOp
                                    | GreaterOp
                                    | GreaterEqualOp
                                    | LessOp
                                    | LessEqualOp
                                    deriving (Show, Eq)

    data UnOperator =               MinusUnOp
                                    | NotOp
                                    deriving (Show, Eq)

    type Location =                 Integer

    data Marker =                   Marker Location
                                    deriving (Show, Eq)

    data Goto =                     Goto Marker
                                    deriving (Show, Eq)

