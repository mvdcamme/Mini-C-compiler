module AST where

    import Control.Monad.State

    import Type

    type Name =                     String

    -- TODO: 
    -- data Definition =               VarDefinition Type Name Exp
    --                                 | FunDefinition Type Name Declarations Body

    data Declaration t =            VarDeclaration Type Name t
                                    | FunDeclaration Type Name (Declarations t) (Body t) t
                                    deriving (Show, Eq)

    type Declarations t =           [Declaration t]

    data Statement t =              AssignStmt (LeftExpression t) (Expression t) t
                                    | BlockStmt (Body t) t
                                    | ExpStmt (Expression t) t
                                    | ForStmt (Statement t) Marker (Expression t) Marker (Statement t) Marker (Body t) Marker t
                                    | IfElseStmt (Expression t) (Body t) Marker (Body t) Marker t
                                    | IfStmt (Expression t) (Body t) Marker t
                                    | Return0Stmt t                                      -- Does not return a value
                                    | Return1Stmt (Expression t) t                       -- Returns a value
                                    | ReadStmt (LeftExpression t) t
                                    | WriteStmt (LeftExpression t) t
                                    | WhileStmt Marker (Expression t) (Body t) Marker t
                                    deriving (Show, Eq)

    type Statements t               = [Statement t]

    data Body t                     = Body (Declarations t) (Statements t) t
                                    deriving (Show, Eq)

    data LeftExpression t           = VariableRefExp Name t
                                    | ArrayRefExp (LeftExpression t) (Expression t) t
                                    | DerefExp (PointerExpression t) t
                                    deriving (Show, Eq)

    data PointerExpression t        = PointerExp (LeftExpression t) (Expression t) t             -- Pointer arithmetic: (lexp + exp), e.g., p + 10
                                    deriving (Show, Eq)

    data Expression t               = LeftExp (LeftExpression t) t
                                    | BinaryExp BinOperator (Expression t) (Expression t) t
                                    | UnaryExp UnOperator (Expression t) t
                                    | UnaryModifyingExp UnModifyingOperator (LeftExpression t) t
                                    | FunctionAppExp Name (Expressions t) t
                                    | FunctionAppWithTypedExp Name [(Expression t, Type)] t
                                    | LengthExp (Expression t) t
                                    | AddressOf (LeftExpression t) t
                                    | NumberExp Integer t
                                    | QCharExp Char t
                                    deriving (Show, Eq)

    getLExpT :: (LeftExpression t) -> t
    getLExpT (VariableRefExp _ t) = t
    getLExpT (ArrayRefExp _ _ t) = t
    getLExpT (DerefExp _ t) = t

    getExpT :: Expression t -> t
    getExpT (LeftExp _ t) = t
    getExpT (BinaryExp _ _ _ t) = t
    getExpT (UnaryExp _ _ t) = t
    getExpT (UnaryModifyingExp _ _ t) = t
    getExpT (FunctionAppExp _ _ t) = t
    getExpT (FunctionAppWithTypedExp _ _ t) = t
    getExpT (LengthExp _ t) = t
    getExpT (AddressOf _ t) = t
    getExpT (NumberExp _ t) = t
    getExpT (QCharExp _ t) = t

    type Expressions t              = [Expression t]

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
                                    | UnModifyingOperator UnModifyingOperator
                                    deriving (Show, Eq)

    data UnModifyingOperator        = PrefixIncOp
                                    | PrefixDecOp
                                    | SuffixIncOp
                                    | SuffixDecOp
                                    deriving (Show, Eq)

    type Location =                 Integer

    data Marker =                   Marker Location
                                    deriving (Show, Eq)

    data Goto =                     Goto Marker
                                    deriving (Show, Eq)

    type WithLocation t =           State Location t

    nextLoc :: WithLocation ()
    nextLoc = do loc <- get
                 put $ loc + 1

