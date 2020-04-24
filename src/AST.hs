module AST where

    import Control.Monad.State

    import Type

    type Name =                     String

    -- TODO: 
    -- data Definition =               VarDefinition Type Name Exp
    --                                 | FunDefinition Type Name Declarations Body

    data Declaration =              VarDeclaration Type Name
                                    | FunDeclaration Type Name Declarations Body
                                    deriving (Show, Eq)

    type Declarations =             [Declaration]

    data Statement =                AssignStmt LeftExpression Expression
                                    | BlockStmt Body
                                    | ExpStmt Expression 
                                    | IfElseStmt Expression Body Marker Body Marker
                                    | IfStmt Expression Body Marker
                                    | Return1Stmt Expression                        -- Returns a value
                                    | Return0Stmt                                   -- Does not return a value
                                    | ReadStmt LeftExpression
                                    | WriteStmt LeftExpression
                                    | WhileStmt Marker Expression Body Marker
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
                                    | UnaryModifyingExp UnModifyingOperator LeftExpression
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

    -- instance Functor Wrapped where
    --   fmap f (Wrapped loc v) = Wrapped loc $ f v

    -- instance Applicative Wrapped where
    --   pure = Wrap
    --   Wrap f <*> Wrap x = Wrap (f x)

    -- instance Monad Wrapped where
    --   return t = TypeSuccess Environment.empty t
    --   (TypeError msg) >>= _ = TypeError msg
    --   (TypeSuccess env typ) >>= _ = TypeError "wefoijqei"
    --   fail msg = TypeError msg

