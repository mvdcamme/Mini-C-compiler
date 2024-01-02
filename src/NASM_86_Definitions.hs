module NASM_86_Definitions where

  import Control.Monad.State
  import Data.Map hiding (map)
  import Debug.Trace

  import ThreeAddressCode as TAC
  import NASM_86

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

  data NASM_86_Compiled_State = NASM_86_Compiled_State { nasmLabelId :: Integer
                                                       , nasmOps :: Operations
                                                       , nasmCurrentLocal :: Address
                                                       , nasmCurrentPar :: Address
                                                       , nasmLocalsMap :: Map Address Address
                                                       , nasmParsMap :: Map Address Address
                                                       , nasmInputToArgs :: Map Input Arg
                                                       , nasmOutputToArgs :: Map Output Arg
                                                       -- , nasmMemsMap :: Map Address Address -- used for debugging
                                                       } deriving (Show, Eq)

  -- data NASM_86_Compiled t   = NASM_86_Compiled { runCompiled :: (NASM_86_Compiled_State -> (t, NASM_86_Compiled_State)) }
  type NASM_86_Compiled t = State NASM_86_Compiled_State t

  -- newNASM_86_Compiled :: t -> NASM_86_Compiled t
  -- newNASM_86_Compiled x = NASM_86_Compiled (\(comp) -> (x, comp))

  newCompiled :: NASM_86_Compiled t -> (t, NASM_86_Compiled_State)
  newCompiled s = 
    let parOffset = 3 * dwordSize -- callee has to save IP, EBP, and EBX
        localOffset = (-1) * dwordSize
        emptyMap = Data.Map.empty
    in (runState s) $ NASM_86_Compiled_State 0 [] localOffset parOffset emptyMap emptyMap emptyMap emptyMap
  -- newCompiled comp =
  --   let f = runCompiled comp
  --       initial = (0, [])
  --   in f initial

  fromOps :: GeneratesOperations t => t -> NASM_86_Compiled ()
  fromOps ops = do state <- get
                   put state { nasmOps = (nasmOps state) ++ (toOperations ops) }

  newLabel :: NASM_86_Compiled Integer
  newLabel = do state <- get
                put state { nasmLabelId = (nasmLabelId state) + 1 }
                return $ nasmLabelId state

  locToMemoryRef' :: Map Address Address -> Address -> Address -> NASM_86_Compiled_State -> NASM_86_Compiled (Address, Map Address Address)
  locToMemoryRef' map k def state' =
    case Data.Map.lookup k map of
      Just val -> return (val, map)
      Nothing  -> do put state'
                     return (def, insert k def map)

  locToMemoryRef :: TACLocation -> NASM_86_Compiled MemoryReference
  locToMemoryRef (TAC.Global address typ) = return . NASM_86.Global address $ typeToSize typ
  locToMemoryRef (TAC.Local address typ) =
    do state <- get
       let currentLocal = nasmCurrentLocal state
       let typeSize = typeToSize typ
       let typeSizeInt = sizeToInteger typeSize
       let mp = nasmLocalsMap state
       (ref, mp') <- locToMemoryRef' mp address currentLocal (state { nasmCurrentLocal = (nasmCurrentLocal state) - typeSizeInt }) -- trace "Adding new local" ((nasmCurrentLocal state) + typeSize) })
       state' <- get
       put state' { nasmLocalsMap = mp' } -- trace ("Local: mp' = " ++ (show mp')) mp' }
       return $ Indirect (trace ("locToMemoryRef of Local " ++ show address ++ " " ++ show typ ++ " -> Indirect " ++ show ref ++ " EBP\nnasmCurrentLocal is now " ++ show (nasmCurrentLocal state'))
                                ref) EBP typeSize
  locToMemoryRef (TAC.Parameter address typ) =
    do state <- get
       let currentPar = nasmCurrentPar state
       let typeSize = typeToSize typ
       let typeSizeInt = sizeToInteger typeSize
       let mp = nasmParsMap state
       (ref, mp') <- locToMemoryRef' mp address currentPar (state { nasmCurrentPar = (nasmCurrentPar state) + typeSizeInt })-- trace "Adding new parameter" ((nasmCurrentPar state) + typeSize) })
       state' <- get
       put state' { nasmParsMap = mp' } -- trace ("Parameter: mp' = " ++ (show mp')) mp' }
       return $ NASM_86.Parameter ref typeSize

  -- class GeneratesNASM_86_Compiled a where
  --   toNASM_86_Compiled :: a -> NASM_86_Compiled ()
  -- instance GeneratesNASM_86_Compiled Operation where
  --   toNASM_86_Compiled op = fromOps op
  -- instance (ToOperation t) => GeneratesNASM_86_Compiled ([] t) where
  --   toNASM_86_Compiled ops = fromOps ops

  -- instance Functor NASM_86_Compiled where
  --   -- fmap :: (a -> b) -> f a -> f b
  --   fmap f (NASM_86_Compiled runCompiled) =
  --     NASM_86_Compiled $ \(s1) -> let (a, s2) = runCompiled s1
  --                                 in (f a, s2)

  -- instance Applicative NASM_86_Compiled where
  --   pure x = newNASM_86_Compiled x
  --   -- (<*>) :: f (a -> b) -> f a -> f b   (with f as NASM_86_Compiled)
  --   (NASM_86_Compiled runCompiled1) <*> (NASM_86_Compiled runCompiled2) =
  --     NASM_86_Compiled $ \(s1) -> let (f, s2) = runCompiled1 s1
  --                                     (a, s3) = runCompiled2 s2
  --                                 in (f a, s3)

  -- instance Monad NASM_86_Compiled where
  --   (NASM_86_Compiled runCompiled1) >>= f =
  --     NASM_86_Compiled $ \(s1) -> let (a, s2) = runCompiled1 s1
  --                                 in (f a, s2)
