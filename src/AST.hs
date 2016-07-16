module AST where

    import Type

    type Name =                     String

    data Declaration =              VarDeclaration Type Name
                                    | FunDeclaration Type Name Declarations Body
                                    deriving (Show, Eq)

    type Declarations =             [Declaration]

    data Statement =                AssignStmt Expression Expression
                                    | BlockStmt Body
                                    | ExpStmt Expression 
                                    | ReturnStmt Expression
                                    | ReadStmt Expression
                                    | WriteStmt Expression
                                    | WhileStmt Marker Expression Marker Body Marker
                                    deriving (Show, Eq)

    type Statements =               [Statement]

    data Body =                     Body Declarations Statements
                                    deriving (Show, Eq)

    data Expression =               VariableRefExp Name
                                    | ArrayRefExp Expression Expression
                                    | BinaryExp BinOperator Expression Expression
                                    | UnaryExp UnOperator Expression
                                    | FunctionAppExp Name Expressions
                                    | LengthExp Expression
                                    | NumberExp Integer
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

