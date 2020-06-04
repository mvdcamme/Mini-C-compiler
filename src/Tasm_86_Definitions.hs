module TASM_86_Definitions where

  import Control.Monad.State
  import Data.Map hiding (map)
  import Debug.Trace

  import ThreeAddressCode as TAC
  import TASM_86

  class ToOperation t where
    toOperation :: t -> Operation
  instance ToOperation Operation where
    toOperation op = op

  class GeneratesOperations a where
    toOperations :: a -> Operations
  instance GeneratesOperations Operation where
    toOperations op = [op]
  instance (ToOperation t) => GeneratesOperations ([] t) where
    toOperations lst = map toOperation lst

  data TASM_86_Compiled_State = TASM_86_Compiled_State { tasmLabelId :: Integer
                                                       , tasmOps :: Operations
                                                       , tasmCurrentLocal :: Address
                                                       , tasmCurrentPar :: Address
                                                       , tasmLocalsMap :: Map Address Address
                                                       , tasmParsMap :: Map Address Address
                                                       } deriving (Show, Eq)

  -- data TASM_86_Compiled t   = TASM_86_Compiled { runCompiled :: (TASM_86_Compiled_State -> (t, TASM_86_Compiled_State)) }
  type TASM_86_Compiled t = State TASM_86_Compiled_State t

  -- newTASM_86_Compiled :: t -> TASM_86_Compiled t
  -- newTASM_86_Compiled x = TASM_86_Compiled (\(comp) -> (x, comp))

  newCompiled :: TASM_86_Compiled t -> (t, TASM_86_Compiled_State)
  newCompiled s = 
    let parOffset = 2 * dwordSize
        localOffset = (-1) * dwordSize
        emptyMap = Data.Map.empty
    in (runState s) $ TASM_86_Compiled_State 0 [] localOffset parOffset emptyMap emptyMap
  -- newCompiled comp =
  --   let f = runCompiled comp
  --       initial = (0, [])
  --   in f initial

  fromOps :: GeneratesOperations t => t -> TASM_86_Compiled ()
  fromOps ops = do state <- get
                   put state { tasmOps = (tasmOps state) ++ (toOperations ops) }

  newLabel :: TASM_86_Compiled Integer
  newLabel = do state <- get
                put state { tasmLabelId = (tasmLabelId state) + 1 }
                return $ tasmLabelId state

  locToMemoryRef' :: Map Address Address -> Address -> Address -> TASM_86_Compiled_State -> TASM_86_Compiled (Address, Map Address Address)
  locToMemoryRef' map k def state' =
    case Data.Map.lookup k map of
      Just val -> return (val, map)
      Nothing  -> do -- put (trace ("Put new state: tasmCurrentLocal = " ++ (show (tasmCurrentLocal state'))) state')
                     put state'
                     -- trace ("Created a new memory ref: ") $ return (def, insert k def map)
                     return (def, insert k def map)

  locToMemoryRef :: TACLocation -> TASM_86_Compiled MemoryReference
  locToMemoryRef (TAC.Global address _) = return $ TASM_86.Global address
  locToMemoryRef (TAC.Local address typ) =
    do state <- get
       let currentLocal = tasmCurrentLocal state
       let typeSize = typeToSize typ
       let typeSizeInt = sizeToInteger typeSize
       let mp = tasmLocalsMap state
       (ref, mp') <- locToMemoryRef' mp address currentLocal (state { tasmCurrentLocal = (tasmCurrentLocal state) - typeSizeInt }) -- trace "Adding new local" ((tasmCurrentLocal state) + typeSize) })
       state' <- get
       put state' { tasmLocalsMap = mp' } -- trace ("Local: mp' = " ++ (show mp')) mp' }
       return $ Indirect ref EBP typeSize
  locToMemoryRef (TAC.Parameter address typ) =
    do state <- get
       let currentPar = tasmCurrentPar state
       let typeSize = typeToSize typ
       let typeSizeInt = sizeToInteger typeSize
       let mp = tasmParsMap state
       (ref, mp') <- locToMemoryRef' mp address currentPar (state { tasmCurrentPar = (tasmCurrentPar state) + typeSizeInt })-- trace "Adding new parameter" ((tasmCurrentPar state) + typeSize) })
       state' <- get
       put state' { tasmParsMap = mp' } -- trace ("Parameter: mp' = " ++ (show mp')) mp' }
       return $ TASM_86.Parameter ref typeSize

  -- class GeneratesTASM_86_Compiled a where
  --   toTASM_86_Compiled :: a -> TASM_86_Compiled ()
  -- instance GeneratesTASM_86_Compiled Operation where
  --   toTASM_86_Compiled op = fromOps op
  -- instance (ToOperation t) => GeneratesTASM_86_Compiled ([] t) where
  --   toTASM_86_Compiled ops = fromOps ops

  -- instance Functor TASM_86_Compiled where
  --   -- fmap :: (a -> b) -> f a -> f b
  --   fmap f (TASM_86_Compiled runCompiled) =
  --     TASM_86_Compiled $ \(s1) -> let (a, s2) = runCompiled s1
  --                                 in (f a, s2)

  -- instance Applicative TASM_86_Compiled where
  --   pure x = newTASM_86_Compiled x
  --   -- (<*>) :: f (a -> b) -> f a -> f b   (with f as TASM_86_Compiled)
  --   (TASM_86_Compiled runCompiled1) <*> (TASM_86_Compiled runCompiled2) =
  --     TASM_86_Compiled $ \(s1) -> let (f, s2) = runCompiled1 s1
  --                                     (a, s3) = runCompiled2 s2
  --                                 in (f a, s3)

  -- instance Monad TASM_86_Compiled where
  --   (TASM_86_Compiled runCompiled1) >>= f =
  --     TASM_86_Compiled $ \(s1) -> let (a, s2) = runCompiled1 s1
  --                                 in (f a, s2)
