module ThreeAddressCode where

  import Data.List.Extra
  import Data.List.Split
  import Debug.Trace

  import AST
  import Environment
  import Type

  type Address =              Integer

  data AtomicValue =          IntValue Integer
                              | CharValue Char
                              deriving (Show, Eq)

  data TACLocation =          Global Address
                              | Local Address
                              | Parameter Address
                              | TACLocationPointsTo TACLocation
                              deriving (Show, Eq)

  data Locations =            Locations { globals :: Address, pars :: Address, locals :: Address, exps :: Address }
                              deriving (Show, Eq)

  type LocEnv =               GenericEnvironment TACLocation

  data Input =                Literal AtomicValue
                              | InAddr TACLocation
                              deriving (Show, Eq)
  data Output =               OutAddr TACLocation
                              deriving (Show, Eq)

  type FunctionName =         String
  type VarName =              String

                              -- Arithmetic
  data TAC =                  AddCode Input Input Output              -- Add
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
                              -- Pointers
                              | AdrCode Input Output                  -- Get address of input
                              | DrfCode Input Output                  -- Dereference address
                              -- Machine operations
                              | MovCode Input Output                  -- Move: basically same as Assign, except the move itself doesn't produce an output. Mostly used to move expressions into slots
                              | ExtCode                               -- Exit program: TODO should get an input for the exit code
                              deriving (Show, Eq)

  type TACs =                 [TAC]

  data CompiledExp          = CompiledExp { expTacs :: TACs, expEnv :: LocEnv, expLocs :: Locations, result :: TACLocation }
  data CompiledStm          = CompiledStm { stmTacs :: TACs, stmEnv :: LocEnv, stmLocs :: Locations }
  data InputTuple4          = InputTuple4 { iptacs :: TACs, ipenv :: LocEnv, iplocs :: Locations, ipinputs :: [Input] }


  data FunctionTAC          = FunctionTAC
                              { fTACName :: FunctionName
                              , fTACNrOfPars :: Integer
                              , fTACNrOfLocals :: Integer
                              , fTACNrOfExps :: Integer
                              , fTACBody :: TACs }
                              deriving (Show, Eq)
  data GlobalVarTAC         = GlobalVarTAC { gvTACAddress :: Address, gvSize :: Integer, gvInitValue :: Maybe Integer }
                              deriving (Show, Eq)
  type FunctionTACs         = [FunctionTAC]
  type GlobalVarTACs        = [GlobalVarTAC]

  data TACFile              = TACFile { globalVarDeclarations :: GlobalVarTACs, functionDefinitions :: FunctionTACs, mainFunctionDefinition :: Maybe FunctionTAC }
                              deriving (Eq)

  instance Show TACFile where
    show (TACFile globals functions main) =
      let globalString = concat . intersperse "\n" $ map show globals
          functionsString = concat . intersperse "\n" $ map show functions
          mainFunctionString = show main
      in "TACFile:\n" ++ globalString ++ "\n" ++ functionsString ++ "\n\n" ++ mainFunctionString

  emptyTACFile :: TACFile
  emptyTACFile = TACFile [] [] Nothing

  printFunName :: FunctionName
  printFunName = "print"

  readFunName :: FunctionName
  readFunName = "read"

  incGlobals :: Locations -> (TACLocation, Locations)
  incGlobals (Locations globals pars locals exps) = (Global globals, Locations (globals + 1) pars locals exps)
  incPars :: Locations -> (TACLocation, Locations)
  incPars (Locations globals pars locals exps) = (Parameter pars, Locations globals (pars + 1) locals exps)
  incLocals :: Locations -> (TACLocation, Locations)
  incLocals (Locations globals pars locals exps) = (Local locals, Locations globals pars (locals + 1) (exps + 1))
  incExps :: Locations -> (TACLocation, Locations)
  incExps (Locations globals pars locals exps) = (Local exps, Locations globals pars locals $ exps + 1)
  -- enterBlock :: Locations -> Locations
  -- enterBlock locs = Locations (globals locs) (pars locs) 0 0
  -- enterFun :: Locations -> Locations
  -- enterFun locs = Locations (globals locs) 0 0 0
  emptyLocations :: Locations
  emptyLocations = Locations 0 0 0 0

  -- newParEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  -- newParEnv env (Locations globals _ _ _) = (Environment.push env, Locations globals 0 0 0)

  -- newFunBodyEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  -- newFunBodyEnv env (Locations globals pars _ _) = (Environment.push env, Locations globals pars 0 0)

  newFunBodyEnv :: Locations -> Locations
  newFunBodyEnv (Locations globals _ _ _) = Locations globals 0 0 0

  newBodyEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  newBodyEnv env locs = (Environment.push env, locs)


  binaryExpToTACs :: LocEnv -> Locations -> Expression -> Expression -> BinOperator -> CompiledExp
  binaryExpToTACs env locs left right op = let CompiledExp leftTACs env1 locs1 leftAddr = expToTACs env locs left
                                               CompiledExp rightTACs env2 locs2 rightAddr = expToTACs env1 locs1 right
                                               (binAddr, locs3) = incExps locs2
                                               makeTAC = case op of
                                                  PlusOp -> AddCode
                                                  MinusOp -> SubCode
                                                  TimesOp -> MulCode
                                                  DivideOp -> DivCode
                                                  EqualOp -> EqlCode
                                                  NequalOp -> NqlCode
                                                  LessOp -> LssCode
                                                  LessEqualOp -> LeqCode
                                                  GreaterOp -> GtrCode
                                                  GreaterEqualOp -> GeqCode
                                               tac = makeTAC (InAddr leftAddr) (InAddr rightAddr) $ OutAddr binAddr
                                           in CompiledExp (leftTACs ++ rightTACs ++ [tac]) env locs3 binAddr

  unaryExpToTACs :: LocEnv -> Locations -> Expression -> UnOperator -> CompiledExp
  unaryExpToTACs env locs exp op =
    let CompiledExp leftTACs env1 locs1 argAddr = expToTACs env locs exp
        (outputAddr, locs2) = incExps locs1
        makeTAC = case op of
                    NotOp     -> NotCode
                    MinusUnOp -> InvCode
        tac = makeTAC (InAddr argAddr) $ OutAddr outputAddr
    in CompiledExp (leftTACs ++ [tac]) env1 locs2 outputAddr

  unaryModifyingExpToTACs :: LocEnv -> Locations -> LeftExpression -> UnModifyingOperator -> CompiledExp
  unaryModifyingExpToTACs env locs lexp op =
    let CompiledExp leftTACs env1 locs1 argAddr = expToTACs env locs (LeftExp lexp)
        (outputAddr, locs2) = incExps locs1
        makeTAC = case op of
                    PrefixIncOp -> PrefixIncCode
                    SuffixIncOp -> SuffixIncCode
                    PrefixDecOp -> PrefixDecCode
                    SuffixDecOp -> SuffixDecCode
        tac = makeTAC (InAddr argAddr) $ OutAddr outputAddr
    in CompiledExp (leftTACs ++ [tac]) env1 locs2 outputAddr

  compileArgs :: LocEnv -> Locations -> Expressions -> CompiledStm
  compileArgs env locs exps = let (env1, locs1, tacs1, inputAddresses) = foldl (\(env, locs, tacs, argInputAddresses) exp -> 
                                                                         let CompiledExp tacs1 env1 locs1 argInputAddr = expToTACs env locs exp
                                                                         in (env1, locs1, (tacs1 ++ tacs), (InAddr argInputAddr : argInputAddresses))) (env, locs, [], []) exps
                              in CompiledStm (tacs1 ++ (map ArgCode inputAddresses)) env1 locs1

  lookupInput :: Name -> LocEnv -> TACLocation -- TODO should produce an error if name cannot be found
  lookupInput name env = let location = case Environment.lookup name env of
                                        Just loc -> loc
                                        _ -> Local (-1)
                         in location

  -- expressions should always produce some input
  atomicToTACs :: LocEnv -> Locations -> AtomicValue -> CompiledExp
  atomicToTACs env locs atomic = let (outputAddr, locs1) = incExps locs
                                     input = Literal atomic
                                     output = OutAddr outputAddr
                                     tac = MovCode input output
                                 in CompiledExp [tac] env locs1 outputAddr

  expToTACs :: LocEnv -> Locations -> Expression -> CompiledExp
  expToTACs env locs atomic@(NumberExp integer) = atomicToTACs env locs $ IntValue integer
  expToTACs env locs atomic@(QCharExp char) = atomicToTACs env locs $ CharValue char
  expToTACs env locs (AddressOf lexp) =
    let (CompiledExp lexpTacs env1 locs1 inAddr) = expToTACs env locs $ LeftExp lexp
        inAddr' = TACLocationPointsTo inAddr
        (outAddr, locs2) = incExps locs1
        tac = AdrCode (InAddr inAddr') (OutAddr outAddr)
        allTacs = lexpTacs `snoc` tac
    in CompiledExp allTacs env1 locs2 outAddr
  expToTACs env locs (BinaryExp op left right) = binaryExpToTACs env locs left right op
  expToTACs env locs (LeftExp (VariableRefExp name)) =
    let address = lookupInput name env
    in CompiledExp [] env locs address
  expToTACs env locs (LeftExp (DerefExp lexp)) =
    let CompiledExp lexpTacs env1 locs1 inAddr = expToTACs env locs $ LeftExp lexp
        (outAddr, locs2) = incExps locs1
        tac = DrfCode (InAddr inAddr) $ OutAddr outAddr
        allTacs = lexpTacs `snoc` tac
    in CompiledExp allTacs env1 locs2 outAddr
  expToTACs env locs (UnaryExp op exp) = unaryExpToTACs env locs exp op
  expToTACs env locs (UnaryModifyingExp op lexp) = unaryModifyingExpToTACs env locs lexp op
  expToTACs env locs (FunctionAppExp name args) = let CompiledStm evalArgsTACs env1 locs1 = compileArgs env locs args 
                                                      -- fnInput = InAddr $ lookupInput name env -- Should use the function's name instead of the address
                                                      (retAddress, locs2) = incExps locs1
                                                      nrOfPars = toInteger $ length args
                                                      callCode = if name == printFunName
                                                                 then PrtCode nrOfPars
                                                                 else CllCode name nrOfPars $ OutAddr retAddress
                                                      allTacs = evalArgsTACs `snoc` callCode
                                                  in CompiledExp allTacs env1 locs2 retAddress

  ifElseStmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  ifElseStmtToTacs env locs (IfElseStmt condExp thenBody m1 elseBody m2) =
    let lbl1 = LblCode m1
        lbl2 = LblCode m2
        (CompiledExp condExpTacs env1 locs1 condExpOutputAddr) = expToTACs env locs condExp
        condTacs = condExpTacs `snoc` JzCode (InAddr condExpOutputAddr) m1
        CompiledStm thenBodyTacs _ locs2 = bodyToTACs env1 locs1 thenBody -- Can ignore the env used in the body
        thenTacs = thenBodyTacs `snoc` JmpCode m2 `snoc` lbl1
        CompiledStm elseBodyTacs _ locs3 = bodyToTACs env1 locs2 elseBody -- Can ignore the env used in the body
        allTacs = condTacs ++ thenTacs ++ elseBodyTacs `snoc` lbl2
    in CompiledStm allTacs env locs3

  ifStmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  ifStmtToTacs env locs (IfStmt condExp thenBody m1) = 
    let lbl1 = LblCode m1
        (CompiledExp condExpTacs env1 locs1 condExpOutputAddr) = expToTACs env locs condExp
        condTacs = condExpTacs `snoc` JzCode (InAddr condExpOutputAddr) m1
        CompiledStm thenBodyTacs _ locs2 = bodyToTACs env1 locs1 thenBody -- Can ignore the env used in the body
        thenTacs = thenBodyTacs `snoc` lbl1
        allTacs = condTacs ++ thenTacs
    in CompiledStm allTacs env locs2

  whileStmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  whileStmtToTacs env locs (WhileStmt m1 condExp body m2) =
    let lbl1 = LblCode m1
        lbl2 = LblCode m2
        (CompiledExp condExpTacs env1 locs1 condExpOutputAddr) = expToTACs env locs condExp
        condTacs = condExpTacs `snoc` JzCode (InAddr condExpOutputAddr) m2
        CompiledStm bodyTacs _ locs2 = bodyToTACs env1 locs1 body -- Can ignore the env used in the body
        allBodyTacs = bodyTacs `snoc` JmpCode m1
        allTacs = lbl1 : condTacs ++ allBodyTacs `snoc` lbl2
    in CompiledStm allTacs env locs2

  forStmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  forStmtToTacs env locs (ForStmt initStmt m1 pred m2 incStmt m3 body m4) =
    let lbl1 = LblCode m1
        lbl2 = LblCode m2
        lbl3 = LblCode m3
        lbl4 = LblCode m4
        CompiledStm initTacs env1 locs1 = stmtToTacs env locs initStmt
        CompiledExp predTacs env2 locs2 predOutAddr = expToTACs env locs pred
        CompiledStm incTacs env3 locs3 = stmtToTacs env2 locs2 incStmt
        -- Can ignore the env used in the body
        CompiledStm bodyTacs _ locs4 = bodyToTACs env3 locs3 body
        -- Init -> Should jump to predicate, but predicate tacs are already placed after the init anuway
        -- Predicate false -> Jump to end of statement
        predFalseJmpTac = JzCode (InAddr predOutAddr) m4
        -- Predicate true -> Jump to body
        predTrueJmpTac = JmpCode m3
        predJmp = [predFalseJmpTac, predTrueJmpTac]
        -- End of body -> Jump to increment tacs
        bodyJmpTac = JmpCode m2
        -- Increment -> Jump to predicate
        incJmpTac = JmpCode m1
        allTacs = initTacs `snoc` lbl1 ++ predTacs ++ predJmp
                  `snoc` lbl2 ++ incTacs `snoc` incJmpTac
                  `snoc` lbl3 ++ bodyTacs `snoc` bodyJmpTac `snoc` lbl4
    in CompiledStm allTacs env3 locs4

  stmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  stmtToTacs env locs (AssignStmt (VariableRefExp name) exp) =
    let (CompiledExp tacs env' locs' outputAddr) = expToTACs env locs exp
        toWrite = lookupInput name env
        input = InAddr outputAddr
        tac = AsnCode input $ OutAddr toWrite
    in CompiledStm (tacs `snoc` tac) env' locs'
  stmtToTacs env locs (AssignStmt (DerefExp lexp) exp) =
    let (CompiledExp lexpTacs env1 locs1 writeAddr) = expToTACs env locs $ LeftExp lexp
        (CompiledExp expTacs env2 locs2 inAddr) = expToTACs env1 locs1 exp
        asnTac = AsnCode (InAddr inAddr) . OutAddr $ TACLocationPointsTo writeAddr
        allTacs = lexpTacs ++ expTacs `snoc` asnTac
    in CompiledStm allTacs env2 locs2
  stmtToTacs env locs (ExpStmt exp) =
    let (CompiledExp tacs env' locs' _) = expToTACs env locs exp
    in CompiledStm tacs env' locs'
  stmtToTacs env locs forStmt@ForStmt{} = forStmtToTacs env locs forStmt
  stmtToTacs env locs ifElseStmt@IfElseStmt{} = ifElseStmtToTacs env locs ifElseStmt
  stmtToTacs env locs ifStmt@IfStmt{} = ifStmtToTacs env locs ifStmt
  stmtToTacs env locs whileStmt@WhileStmt{} = whileStmtToTacs env locs whileStmt
  stmtToTacs env locs Return0Stmt = CompiledStm [Rt0Code] env locs
  stmtToTacs env locs (Return1Stmt exp) =
    let (CompiledExp expTacs env' locs' outputAddr) = expToTACs env locs exp
        retTac = Rt1Code $ InAddr outputAddr
    in CompiledStm (expTacs `snoc` retTac) env' locs'
  -- stmtToTacs env locs _ = CompiledStm [] env locs

  addDeclarations :: LocEnv -> Locations -> (Locations -> (TACLocation, Locations)) -> (Address -> TACLocation) -> Declarations -> (LocEnv, Locations)
  addDeclarations env locs inc toLoc decls =
    foldl (\(env, locs) name ->
            let (address, locs1) = inc locs
                env1 = Environment.insert name address env
            in (env1, locs1))
          (env, locs) $ map (\(VarDeclaration _ name) -> name) decls

  globalVarDefToTACs :: LocEnv -> Locations -> Declaration -> CompiledStm
  globalVarDefToTACs env locs (VarDeclaration _ name) = let (address, updatedLocs) = incGlobals locs
                                                            updatedEnv = Environment.insert name address env
                                                        in CompiledStm [] updatedEnv updatedLocs -- No TACs needed

  bodyToTACs :: LocEnv -> Locations -> Body -> CompiledStm
  bodyToTACs env locs (Body bodyDecls bodyStmts) =
    let (bodyPushedEnv, locs') = newBodyEnv env locs
        (bodyEnv, bodyLocs) = addDeclarations bodyPushedEnv locs' incLocals Local bodyDecls
        (bodyEnv', bodyLocs', bodyTacs) = foldl (\(env, locs, tacs) stm -> 
          let CompiledStm tacs1 env1 locs1 = stmtToTacs env locs stm
          -- in (env1, locs1, tacs ++ [DebugLocs locs1] ++ tacs1)) (bodyEnv, bodyLocs, []) bodyStmts
          in (env1, locs1, tacs ++ tacs1)) (bodyEnv, bodyLocs, []) bodyStmts
    in CompiledStm bodyTacs bodyEnv' bodyLocs'

  declarationToTACs :: LocEnv -> Locations -> Declaration -> CompiledStm
  declarationToTACs env locs decl@(VarDeclaration _ _) = globalVarDefToTACs env locs decl
  declarationToTACs env locs (FunDeclaration _ name pars body) = 
    let (address, updatedLocs) = incGlobals locs -- Add function as a Global loc
        updatedLocs' = newFunBodyEnv updatedLocs
        addedEnv = Environment.insert name address env
        parsPushedEnv = Environment.push env
        (parsEnv, parsLocs) = foldl (\(env, locs) (VarDeclaration _ parName) -> 
                                       let (parAddr, locs1) = incPars locs
                                           parAddedEnv = Environment.insert parName parAddr env
                                       in (parAddedEnv, locs1)) (parsPushedEnv, updatedLocs') pars
        CompiledStm bodyTacs bodyEnv' bodyLocs' = bodyToTACs parsEnv parsLocs body
        fullFunctionTacs = bodyTacs
    in CompiledStm fullFunctionTacs addedEnv bodyLocs' -- Use addedEnv instead of bodyEnv', since we don't need the two pushed frames in the rest of the global scope

  defaultGlobalVarSize :: Integer
  defaultGlobalVarSize = 4

  generateTACs' :: LocEnv -> Locations -> Declarations -> TACFile -> TACFile
  generateTACs' env locs [] acc = acc
  generateTACs' env locs (decl@(VarDeclaration _ _):decls) (TACFile globalVars functions main) =
    let globalVar = GlobalVarTAC (globals locs) defaultGlobalVarSize Nothing
        CompiledStm tacs1 env1 locs1 = declarationToTACs env locs decl
        newAcc = TACFile (globalVars `snoc` globalVar) functions main
    in generateTACs' env1 locs1 decls newAcc
  generateTACs' env locs (decl@(FunDeclaration _ name _ _):decls) (TACFile globalVars functions main)   =
    let CompiledStm tacs1 env1 locs1 = declarationToTACs env locs decl
        nrOfPars = pars locs1
        nrOfLocals = locals locs1
        nrOfExps = exps locs1
        functionTAC = FunctionTAC name nrOfPars nrOfLocals nrOfExps tacs1
        newAcc =  if name == "main"
                  then TACFile globalVars functions $ Just functionTAC
                  else TACFile globalVars (functions `snoc` functionTAC) main
    in generateTACs' env1 locs1 decls newAcc

  transformTACFile :: TACFile -> TACFile
  transformTACFile (TACFile decls functions (Just (FunctionTAC fTACName fTACNrOfPars fTACNrOfLocals fTACNrOfExps fTACBody))) =
    let replaceRet = \tac -> case tac of Rt0Code -> ExtCode; other -> other
        replacedTACs = map replaceRet fTACBody
    in TACFile decls functions . Just $ FunctionTAC fTACName fTACNrOfPars fTACNrOfLocals fTACNrOfExps replacedTACs
  transformTACFile tacFile = tacFile

  generateTACs :: Declarations -> TACFile
  generateTACs decls = transformTACFile $ generateTACs' Environment.empty emptyLocations decls emptyTACFile

  -- findTACsBelonging :: TACs -> TACs -> (TACs, TACs)
  -- findTACsBelonging acc [] = (reverse acc, [])
  -- findTACsBelonging acc (EndFunCode _ : tacs) = (reverse acc, tacs)
  -- findTACsBelonging acc tac:tacs = findTACsBelonging (tac : acc) tacs

  -- tacsToFunctionTAC :: FunctionTACs -> TACs -> FunctionTACs
  -- tacsToFunctionTAC acc [] = reverse acc
  -- tacsToFunctionTAC acc (StartFunCode name : tacs) =
  --   let (fTacs, remainingTacs) = findTACsBelonging [] tacs
  --       functionTAC = FunctionTAC name slots fTacs
  --   in tacsToFunctionTAC (functionTAC : acc) remainingTacs

  -- tacsToFunctionTAC :: TACs -> FunctionTACs
  -- tacsToFunctionTAC [] = 

  -- tacsToTACFile :: TACs -> Locations -> TACFile
  -- tacsToTACFile tacs locs = let makeDefaultGlobalVar address = GlobalVarTAC address defaultGlobalVarSize None
  --                               globalVarTACS = map makeDefaultGlobalVar [0 .. (globals locs - 1)]
  --                               belongToFunction = splitWhen (\tac -> case tac of
  --                                                                       EndFunCode _ -> True
  --                                                                       _ -> False) tacs
  --                               removeBeginFunCode = filter (\tac -> case tac of
  --                                                                      BeginFunCode _ -> False
  --                                                                      _ -> True) belongToFunction
  --                           in TACFile globalVarTACS

  -- generateTACFile :: Declarations -> TACFile
  -- generateTACFile decls = let (tacs, locs) = generateTACs decls
  --                         in tacsToTACFile tacs locs