module TypeChecking where

    import Data.Map

    import AST
    import Type

    data Environment = Map Name Type

    typeOfExp :: Environment -> Expression -> Type
    typeOfExp env exp = case exp of
        NumberExp _ -> Atom IntType