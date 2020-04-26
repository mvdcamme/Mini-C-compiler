module TASM_86_Definitions where
  import Control.Monad.State

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

  type Something = (Integer, Operations)
  -- data TASM_86_Compiled t   = TASM_86_Compiled { runCompiled :: (Something -> (t, Something)) }
  type TASM_86_Compiled t = State Something t

  -- newTASM_86_Compiled :: t -> TASM_86_Compiled t
  -- newTASM_86_Compiled x = TASM_86_Compiled (\(comp) -> (x, comp))

  newCompiled :: TASM_86_Compiled t -> (t, Something)
  newCompiled s = (runState s) (0, [])
  -- newCompiled comp =
  --   let f = runCompiled comp
  --       initial = (0, [])
  --   in f initial

  fromOps :: GeneratesOperations t => t -> TASM_86_Compiled ()
  fromOps ops = do (id, oldOps) <- get
                   put (id, oldOps ++ (toOperations ops))

  newLabel :: TASM_86_Compiled Integer
  newLabel = do (id, ops) <- get
                put (id + 1, ops)
                return id

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
