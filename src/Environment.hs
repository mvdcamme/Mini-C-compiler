module Environment where 

    import Data.Map

    import AST
    import Type

    type Environment = [Map Name Type]

    lookup :: Name -> Environment -> Maybe Type
    lookup var [] = Nothing
    lookup var (frame : rest) = case Data.Map.lookup var frame of
        Just result -> Just result
        Nothing -> Environment.lookup var rest

    insert :: Name -> Type -> Environment -> Environment
    insert var typ (frame : rest) = (Data.Map.insert var typ frame) : rest

    push :: Environment -> Environment
    push env = Data.Map.empty : env

    pop :: Environment -> Environment
    pop (frame : rest) = rest