module Environment where 

    import Debug.Trace
    import Data.Map

    type GenericEnvironment v = [Map String v]

    empty :: GenericEnvironment v
    empty = [Data.Map.empty]

    lookup :: String -> GenericEnvironment v -> Maybe v
    -- lookup var env | Debug.Trace.trace ("lookup " ++ show var ++ " " ++ show env) False = undefined
    lookup var [] = Nothing
    lookup var (frame : rest) = case Data.Map.lookup var frame of
        Just result -> Just result
        Nothing -> Environment.lookup var rest

    insert :: String -> v -> GenericEnvironment v -> GenericEnvironment v
    -- insert var typ env | Debug.Trace.trace ("inserting " ++ show var ++ " " ++ show env) False = undefined
    insert var typ (frame : rest) = (Data.Map.insert var typ frame) : rest
    insert var typ [] = [] -- TODO Should be an error!!

    push :: GenericEnvironment v -> GenericEnvironment v
    push env = Data.Map.empty : env

    pop :: GenericEnvironment v -> GenericEnvironment v
    pop (frame : rest) = rest