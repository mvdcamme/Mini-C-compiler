module ThreeAddressCode where

  import Control.Monad.State
  import Data.List.Extra
  import Data.List.Split
  import Data.Map hiding (map) 
  import Debug.Trace

  import AST
  import Environment
  import Type

  type Address =              Integer

  data AtomicValue =          IntValue Integer
                              | CharValue Char
                              deriving (Show, Eq)

  data TACLocation =          Global Address Type
                              | Local Address Type
                              | Parameter Address Type
                              | TACLocationPointsTo TACLocation
                              deriving (Show, Eq)

  data Locations =            Locations { globals :: Address
                                        , pars :: Address
                                        , locals :: Address
                                        , exps :: Address
                                        } deriving (Show, Eq)

  type LocEnv =               GenericEnvironment TACLocation

  data Input =                Literal AtomicValue
                              | InAddr TACLocation
                              deriving (Show, Eq)
  data Output =               OutAddr TACLocation
                              deriving (Show, Eq)

  type FunctionName =         String
  type VarName =              String

                              -- Arithmetic
  data TAC                  =   AddCode Input Input Output              -- Add
                              | SubCode Input Input Output            -- Subtract
                              | MulCode Input Input Output            -- Multiply
                              | DivCode Input Input Output            -- Division
                              | InvCode Input Output                  -- Inverse
                              | PrefixIncCode Input Output            -- Prefix Increment
                              | SuffixIncCode Input Output            -- Suffix Increment
                              | PrefixDecCode Input Output            -- Prefix Decrement
                              | SuffixDecCode Input Output            -- Suffix Decrement
                              -- Comparison
                              | EqlCode Input Input Output            -- Equal
                              | NqlCode Input Input Output            -- Not equal
                              | LssCode Input Input Output            -- Less than
                              | LeqCode Input Input Output            -- Less than / Equal
                              | GtrCode Input Input Output            -- Greater than
                              | GeqCode Input Input Output            -- Greater than / Equal
                              -- Logical operators
                              | AndCode Input Input                   -- Logical And
                              | OrCode Input Input                    -- Logical Or
                              | NotCode Input Output                  -- Logical Not
                              | XorCode Input Output                  -- Logical Exclusive Or
                              -- Labels and jumps
                              | LblCode Marker                        -- Define label with marker
                              | JmpCode Marker                        -- Unconditional jump
                              | JnzCode Input Marker                  -- Jump if input does not equal zero
                              | JzCode Input Marker                   -- Jump if input equals zero
                              -- Function calls and returns
                              | ArgCode Input                         -- Pass argument to function
                              | CllCode FunctionName Integer Output   -- Call function
                              | PrtCode Integer                       -- Call to print function
                              | Rt0Code                               -- Return from function without value
                              | Rt1Code Input                         -- Return from function with value
                              -- | RvpCode Integer                    -- Remove Integers pars from the stack after calling a function
                              -- Assignments
                              | AsnCode Input Output                  -- Assign: should also have an output if the assignment itself produces a value
                              -- Casting
                              | CstCode Input Output                  -- Cast: input type to output type
                              -- Pointers
                              | AdrCode Input Output                  -- Get address of input
                              | DrfCode Input Output                  -- Dereference address
                              -- Machine operations
                              | MovCode Input Output                  -- Move: basically same as Assign, except the move itself doesn't produce an output. Mostly used to move expressions into slots
                              | ExtCode                               -- Exit program: TODO should get an input for the exit code
                              deriving (Show, Eq)

  type TACs                 = [TAC]

  type LocalAddresses       = Map Address Type

  data TACStateState        = TACStateState { tacs :: TACs
                                            , locEnv :: LocEnv
                                            , locs :: Locations
                                            , localAddresses :: LocalAddresses }
                              deriving (Show, Eq)

  type TACState a           = State TACStateState a
  type CompiledExp          = TACState TACLocation
  type CompiledStm          = TACState ()

  data FunctionTAC          = FunctionTAC { fTACName :: FunctionName
                                          , fTACNrOfPars :: Integer
                                          , fTACNrOfLocals :: Integer
                                          , fTACNrOfExps :: Integer
                                          , fTACBody :: TACs
                                          , fTACLocalAddresses :: LocalAddresses
                                        } deriving (Show, Eq)
  data GlobalVarTAC         = GlobalVarTAC { gvTACAddress :: Address
                                           , gvSize :: Integer
                                           , gvInitValue :: Maybe Integer
                                           } deriving (Show, Eq)
  type FunctionTACs         = [FunctionTAC]
  type GlobalVarTACs        = [GlobalVarTAC]

  data TACFile              = TACFile { globalVarDeclarations :: GlobalVarTACs
                                      , functionDefinitions :: FunctionTACs
                                      , mainFunctionDefinition :: Maybe FunctionTAC
                                      } deriving (Eq)

  instance Show TACFile where
    show (TACFile globals functions main) =
      let globalString = concat . intersperse "\n" $ map show globals
          functionsString = concat . intersperse "\n" $ map show functions
          mainFunctionString = show main
      in "TACFile:\n" ++ globalString ++ "\n" ++ functionsString ++ "\n\n" ++ mainFunctionString

  class HasType a where
    getType :: a -> Type

  instance HasType TACLocation where
    getType (Global _ typ) = typ
    getType (Local _ typ) = typ
    getType (Parameter _ typ) = typ
    getType (TACLocationPointsTo loc) = PointerType $ getType loc

  instance HasType Input where
    getType Literal{} = Atom lowestAtomicType
    getType (InAddr loc) = getType loc

  instance HasType Output where
    getType (OutAddr loc) = getType loc

  emptyTACFile :: TACFile
  emptyTACFile = TACFile [] [] Nothing

  printFunName :: FunctionName
  printFunName = "print"

  readFunName :: FunctionName
  readFunName = "read"

  incGlobals :: Type -> TACState TACLocation
  incGlobals typ =
    do state <- get
       let locs' = (locs state) { globals = (globals $ locs state) + 1 }
       put state{locs = locs'}
       return $ Global (globals $ locs state) typ -- Uses old "current" global address
  incPars :: Type -> TACState TACLocation
  incPars typ =
    do state <- get
       let locs' = (locs state) { pars = (pars $ locs state) + 1 }
       put state{locs = locs'}
       return $ Parameter (pars $ locs state) typ
  incLocals :: Type -> TACState TACLocation
  incLocals typ =
    do state <- get
       let newAddr = locals $ locs state
       let locs' = (locs state) { locals = newAddr + 1, exps = (exps $ locs state) + 1 }
       let locals' = Data.Map.insert newAddr typ $ localAddresses state
       put state{locs = locs', localAddresses = locals'}
       return $ Local newAddr typ
  incExps :: Type -> TACState TACLocation
  incExps typ =
    do state <- get
       let newAddr = exps $ locs state
       let locs' = (locs state) { exps = newAddr + 1 }
       let locals' = Data.Map.insert newAddr typ $ localAddresses state
       put state{locs = locs', localAddresses = locals'}
       return $ Local newAddr typ

  addTAC :: TAC -> TACState ()
  addTAC tac =
    do state <- get
       put state{tacs = (tacs state) `snoc` tac}

  addTACs :: TACs -> TACState ()
  addTACs tacs' =
    do state <- get
       put state{tacs = (tacs state) ++ tacs'}

  emptyLocations :: Locations
  emptyLocations = Locations 0 0 0 0

  newFunBodyEnv :: TACState ()
  newFunBodyEnv =
    do state <- get
       let locs' = Locations (globals $ locs state) 0 0 0
       put state{locs = locs'}

  newBodyEnv :: TACState ()
  newBodyEnv =
    do state <- get
       put state{locEnv = Environment.push $ locEnv state}

  popEnv :: TACState ()
  popEnv =
    do state <- get
       put state{locEnv = Environment.pop $ locEnv state}

  insertIntoEnv :: VarName -> TACLocation -> TACState ()
  insertIntoEnv name address =
    do state <- get
       let env' = Environment.insert name address $ locEnv state
       put state {locEnv = env'}

  castArgToAddress :: TACLocation -> Type -> TACState TACLocation -> TACState TACLocation
  castArgToAddress loc typ makeOut | getType loc /= typ =
    do out <- makeOut
       addTAC . CstCode (InAddr loc) $ OutAddr out
       return out
  castArgToAddress loc _ _ | otherwise = return loc

  -- Cast input to given type, if it doesn't already have the same type
  castArg :: TACLocation -> Type -> TACState TACLocation
  -- TODO: Should try to avoid generating an explicit cast operation
  -- if the input is a literal. We now only avoid this when the output
  -- type is already a char, since literals are of this type. 
  -- Update: Is this comment still true?
  castArg loc typ = castArgToAddress loc typ $ incExps typ

  -- Generate TACs for evaluating expression and cast to given type if necessary
  castExpToTACS :: Expression Type -> Type -> CompiledExp
  castExpToTACS exp typ =
    do outAddr <- expToTACs exp
       castedOutAddr <- castArg outAddr typ
       return castedOutAddr

  binaryExpToTACs :: Expression Type -> Expression Type -> BinOperator -> Type -> CompiledExp
  binaryExpToTACs left right op typ =
    do leftAddr <- castExpToTACS left typ
       rightAddr <- castExpToTACS right typ
       binAddr <- incExps typ
       let makeTAC = case op of {
         PlusOp -> AddCode;
         MinusOp -> SubCode;
         TimesOp -> MulCode;
         DivideOp -> DivCode;
         EqualOp -> EqlCode;
         NequalOp -> NqlCode;
         LessOp -> LssCode;
         LessEqualOp -> LeqCode;
         GreaterOp -> GtrCode;
         GreaterEqualOp -> GeqCode;
       }
       let tac = makeTAC (InAddr leftAddr) (InAddr rightAddr) $ OutAddr binAddr
       addTAC tac
       return binAddr

  unaryExpToTACs :: Expression Type -> UnOperator -> Type -> CompiledExp
  unaryExpToTACs exp op typ =
    do argAddr <- castExpToTACS exp typ
       outputAddr <-incExps typ
       let makeTAC = case op of {
         NotOp     -> NotCode;
         MinusUnOp -> InvCode;
       }
       let tac = makeTAC (InAddr argAddr) $ OutAddr outputAddr
       addTAC tac
       return outputAddr

  unaryModifyingExpToTACs :: LeftExpression Type -> UnModifyingOperator -> Type -> CompiledExp
  unaryModifyingExpToTACs lexp op typ =
    do inAddr <- castExpToTACS (LeftExp lexp $ getLExpT lexp) typ
       outputAddr <- incExps typ
       let makeTAC = case op of {
         PrefixIncOp -> PrefixIncCode;
         SuffixIncOp -> SuffixIncCode;
         PrefixDecOp -> PrefixDecCode;
         SuffixDecOp -> SuffixDecCode;
       }
       let tac = makeTAC (InAddr inAddr) $ OutAddr outputAddr
       addTAC tac
       return outputAddr

  compileArgs :: Expressions Type -> CompiledStm
  compileArgs exps =
    do inputAddresses <- foldM (\acc exp -> 
               do argInputAddr <- expToTACs exp
                  return $ (InAddr argInputAddr : acc)) ([] :: [Input]) exps
       addTACs $ (map ArgCode inputAddresses)

  lookupInput :: Name -> TACState TACLocation -- TODO should produce an error if name cannot be found
  lookupInput name =
    do state <- get
       case Environment.lookup name $ locEnv state of
         Just loc -> return loc
         _ -> fail ("Variable " ++ (show name) ++ " not in environment")

  -- expressions should always produce some input
  atomicToTACs :: AtomicValue -> AtomicType -> CompiledExp
  atomicToTACs atomic atomType =
    do let typ = Atom atomType
       outputAddr <- incExps typ
       let input = Literal atomic
       let output = OutAddr outputAddr
       let tac = MovCode input output
       addTAC tac
       return outputAddr

  expToTACs :: Expression Type -> CompiledExp
  expToTACs atomic@(NumberExp integer (Atom typ)) = atomicToTACs (IntValue integer) typ
  expToTACs atomic@(QCharExp char (Atom typ)) = atomicToTACs (CharValue char) typ
  expToTACs (AddressOf lexp typ) =
    do inAddr <- expToTACs . LeftExp lexp $ getLExpT lexp
       let inAddr' = TACLocationPointsTo inAddr
       outAddr <- incExps typ
       let tac = AdrCode (InAddr inAddr') $ OutAddr outAddr
       addTAC tac
       return outAddr
  expToTACs (BinaryExp op left right typ) = binaryExpToTACs left right op typ
  expToTACs (LeftExp (VariableRefExp name _) typ) =
    do address <- lookupInput name
       return address
  expToTACs (LeftExp (DerefExp lexp _) typ) =
    do inAddr <- expToTACs . LeftExp lexp $ getLExpT lexp
       outAddr <- incExps typ
       let tac = DrfCode (InAddr inAddr) $ OutAddr outAddr
       addTAC tac
       return outAddr
  expToTACs (UnaryExp op exp typ) = unaryExpToTACs exp op typ
  expToTACs (UnaryModifyingExp op lexp typ) = unaryModifyingExpToTACs lexp op typ
  expToTACs (FunctionAppExp name args typ) =
    do compileArgs args 
       retAddress <- incExps typ
       let nrOfPars = toInteger $ length args
       let callCode = if name == printFunName
                         then PrtCode nrOfPars
                         else CllCode name nrOfPars $ OutAddr retAddress
       addTAC callCode
       return retAddress

  ifElseStmtToTacs :: Statement Type -> CompiledStm
  ifElseStmtToTacs (IfElseStmt condExp thenBody m1 elseBody m2 _) =
    do let lbl1 = LblCode m1
       let lbl2 = LblCode m2
       condExpOutputAddr <- expToTACs condExp
       addTAC $ JzCode (InAddr condExpOutputAddr) m1
       bodyToTACs thenBody -- Can ignore the env used in the body
       addTACs [JmpCode m2, lbl1]
       bodyToTACs elseBody -- Can ignore the env used in the body
       addTAC lbl2

  ifStmtToTacs :: Statement Type -> CompiledStm
  ifStmtToTacs (IfStmt condExp thenBody m1 _) = 
    do let lbl1 = LblCode m1
       condExpOutputAddr <- expToTACs condExp
       addTAC $ JzCode (InAddr condExpOutputAddr) m1
       bodyToTACs thenBody -- Can ignore the env used in the body
       addTAC lbl1

  whileStmtToTacs :: Statement Type -> CompiledStm
  whileStmtToTacs (WhileStmt m1 condExp body m2 _) =
    do let lbl1 = LblCode m1
       let lbl2 = LblCode m2
       addTAC lbl1
       condExpOutputAddr <- expToTACs condExp
       addTAC $ JzCode (InAddr condExpOutputAddr) m2
       bodyToTACs body -- Can ignore the env used in the body
       addTAC $ JmpCode m1
       addTAC lbl2

  forStmtToTacs :: Statement Type -> CompiledStm
  forStmtToTacs (ForStmt initStmt m1 pred m2 incStmt m3 body m4 _) =
    do let lbl1 = LblCode m1
       let lbl2 = LblCode m2
       let lbl3 = LblCode m3
       let lbl4 = LblCode m4
       stmtToTacs initStmt
       addTAC lbl1
       predOutAddr <- expToTACs pred
       -- Predicate false -> Jump to end of statement
       let predFalseJmpTac = JzCode (InAddr predOutAddr) m4
       -- Predicate true -> Jump to body
       let predTrueJmpTac = JmpCode m3
       let predJmp = [predFalseJmpTac, predTrueJmpTac]
       addTACs predJmp
       addTAC lbl2
       stmtToTacs incStmt
       -- Increment -> Jump to predicate
       let incJmpTac = JmpCode m1
       addTAC incJmpTac
       addTAC lbl3
       -- Can ignore the env used in the body
       bodyToTACs body
       -- Init -> Should jump to predicate, but predicate tacs are already placed after the init anuway
       -- End of body -> Jump to increment tacs
       let bodyJmpTac = JmpCode m2
       addTAC bodyJmpTac
       addTAC lbl4
       -- allTacs = initTacs `snoc` lbl1 ++ predTacs ++ predJmp
       --           `snoc` lbl2 ++ incTacs `snoc` incJmpTac
       --           `snoc` lbl3 ++ bodyTacs `snoc` bodyJmpTac `snoc` lbl4

  stmtToTacs :: Statement Type -> CompiledStm
  stmtToTacs (AssignStmt (VariableRefExp name typ) exp _) =
    do inAddr <- expToTACs exp
       toWrite <- lookupInput name
       let input = InAddr inAddr
       -- castArgToAddress inAddr typ (return toWrite)
       -- return ()
       let tac = AsnCode input $ OutAddr toWrite -- Should do a cast is the types don't match
       addTAC tac
  stmtToTacs (AssignStmt (DerefExp lexp typ) exp _) =
    do writeAddr <- expToTACs . LeftExp lexp $ getLExpT lexp
       inAddr <- expToTACs exp
       -- Should do a cast is the types don't match
       -- castArgToAddress inAddr typ (return $ TACLocationPointsTo writeAddr)
       -- return ()
       let asnTac = AsnCode (InAddr inAddr) . OutAddr $ TACLocationPointsTo writeAddr
       addTAC asnTac
  stmtToTacs (ExpStmt exp _) =
    do expToTACs exp
       return ()
  stmtToTacs forStmt@ForStmt{} = forStmtToTacs forStmt
  stmtToTacs ifElseStmt@IfElseStmt{} = ifElseStmtToTacs ifElseStmt
  stmtToTacs ifStmt@IfStmt{} = ifStmtToTacs ifStmt
  stmtToTacs whileStmt@WhileStmt{} = whileStmtToTacs whileStmt
  stmtToTacs Return0Stmt{} = addTAC Rt0Code
  stmtToTacs (Return1Stmt exp typ) =
    do outputAddr <- expToTACs exp
       let retTac = Rt1Code $ InAddr outputAddr
       addTAC retTac
  -- stmtToTacs _ = return ()

  addDeclarations :: (Type -> TACState TACLocation) -> (Declarations Type) -> TACState ()
  addDeclarations inc decls =
    do mapM (\tuple ->
              do let (name, typ) = tuple
                 address <- inc typ
                 state <- get
                 insertIntoEnv name address)
            (map (\(VarDeclaration typ name _) -> (name, typ)) decls)
       return ()

  globalVarDefToTACs :: Declaration Type -> CompiledStm
  globalVarDefToTACs (VarDeclaration typ name _) =
    do address <- incGlobals typ
       insertIntoEnv name address

  bodyToTACs :: Body Type -> CompiledStm
  bodyToTACs (Body bodyDecls bodyStmts _) =
    do newBodyEnv
       addDeclarations incLocals bodyDecls
       mapM stmtToTacs bodyStmts
       popEnv

  declarationToTACs :: Declaration Type -> CompiledStm
  declarationToTACs decl@VarDeclaration{} = globalVarDefToTACs decl
  declarationToTACs (FunDeclaration typ name pars body _) = 
    do newFunBodyEnv
       address <- incGlobals typ -- Add function as a Global loc
       insertIntoEnv name address
       newBodyEnv
       mapM (\(VarDeclaration typ parName _) -> 
              do parAddr <- incPars typ
                 insertIntoEnv parName parAddr
            ) pars
       bodyToTACs body
       popEnv
    -- in CompiledStm fullFunctionTacs addedEnv bodyLocs' -- Use addedEnv instead of bodyEnv', since we don't need the two pushed frames in the rest of the global scope

  defaultGlobalVarSize :: Integer
  defaultGlobalVarSize = 4

  generateTACs' :: LocEnv -> Locations -> Declarations Type -> TACFile -> TACFile
  generateTACs' _ _ [] acc = acc
  generateTACs' locEnv locs (decl@VarDeclaration{}:decls) (TACFile globalVars functions main) =
    let globalAddr = globals locs
        globalVar = GlobalVarTAC globalAddr defaultGlobalVarSize Nothing
        newState = TACStateState [] locEnv locs Data.Map.empty
        (_, TACStateState _ locEnv' locs' localAddresses) = runState (declarationToTACs decl) newState
        newAcc = TACFile (globalVars `snoc` globalVar) functions main
    in generateTACs' locEnv' locs' decls newAcc
  generateTACs' locEnv locs (decl@(FunDeclaration _ name _ _ _):decls) (TACFile globalVars functions main)   =
    let newState = TACStateState [] locEnv locs Data.Map.empty
        (_, TACStateState tacs locEnv' locs' localAddresses) = runState (declarationToTACs decl) newState
        nrOfPars = pars locs'
        nrOfLocals = locals locs'
        nrOfExps = exps locs'
        functionTAC = FunctionTAC name nrOfPars nrOfLocals nrOfExps tacs localAddresses
        newAcc =  if name == "main"
                  then TACFile globalVars functions $ Just functionTAC
                  else TACFile globalVars (functions `snoc` functionTAC) main
    in generateTACs' locEnv' locs' decls newAcc

  transformTACFile :: TACFile -> TACFile
  transformTACFile (TACFile decls functions (Just functionTAC)) =
    let replaceRet = \tac -> case tac of Rt0Code -> ExtCode; other -> other
        replacedTACs = map replaceRet $ fTACBody functionTAC
        newTACFile = functionTAC{fTACBody = replacedTACs}
    in TACFile decls functions $ Just newTACFile
  transformTACFile tacFile = tacFile

  generateTACs :: (Declarations Type) -> TACFile
  generateTACs decls = transformTACFile $ generateTACs' Environment.empty emptyLocations decls emptyTACFile