module Environment where 

    import Debug.Trace
    import Data.Map

    import AST
    import Type

    type Environment = [Map Name Type]

    empty :: Environment
    empty = [Data.Map.empty]

    lookup :: Name -> Environment -> Maybe Type
    -- lookup var env | Debug.Trace.trace ("lookup " ++ show var ++ " " ++ show env) False = undefined
    lookup var [] = Nothing
    lookup var (frame : rest) = case Data.Map.lookup var frame of
        Just result -> Just result
        Nothing -> Environment.lookup var rest

    insert :: Name -> Type -> Environment -> Environment
    -- insert var typ env | Debug.Trace.trace ("inserting " ++ show var ++ " " ++ show env) False = undefined
    insert var typ (frame : rest) = (Data.Map.insert var typ frame) : rest
    insert var typ [] = [] -- TODO Should be an error!!

    push :: Environment -> Environment
    push env = Data.Map.empty : env

    pop :: Environment -> Environment
    pop (frame : rest) = rest