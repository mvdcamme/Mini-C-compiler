module TASM_86_Definitions where
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

  data TASM_86_Compiled t   = TASM_86_Compiled Integer Operations t

  newTASM_86_Compiled :: t -> TASM_86_Compiled t
  newTASM_86_Compiled x = TASM_86_Compiled 0 [] x

  newCompiled :: TASM_86_Compiled ()
  newCompiled = newTASM_86_Compiled ()

  fromOps :: GeneratesOperations t => t -> TASM_86_Compiled ()
  fromOps ops = TASM_86_Compiled 0 (toOperations ops) ()

  -- class GeneratesTASM_86_Compiled a where
  --   toTASM_86_Compiled :: a -> TASM_86_Compiled ()
  -- instance GeneratesTASM_86_Compiled Operation where
  --   toTASM_86_Compiled op = fromOps op
  -- instance (ToOperation t) => GeneratesTASM_86_Compiled ([] t) where
  --   toTASM_86_Compiled ops = fromOps ops

  instance Functor TASM_86_Compiled where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (TASM_86_Compiled id ops x) =
      TASM_86_Compiled id ops $ f x

  instance Applicative TASM_86_Compiled where
    pure x = newTASM_86_Compiled x
    -- (<*>) :: f (a -> b) -> f a -> f b   (with f as TASM_86_Compiled)
    (TASM_86_Compiled id1 ops1 f) <*> (TASM_86_Compiled id2 ops2 x) =
      TASM_86_Compiled (id1 + id2) (ops1 ++ ops2) $ f x

  instance Monad TASM_86_Compiled where
    (TASM_86_Compiled id1 ops1 x1) >>= f =
      let (TASM_86_Compiled id2 ops2 x2) = f x1
      in TASM_86_Compiled (id1 + id2) (ops1 ++ ops2) x2

  newLabel :: TASM_86_Compiled t -> TASM_86_Compiled Integer
  newLabel (TASM_86_Compiled id ops x) = TASM_86_Compiled (id + 1) ops id
