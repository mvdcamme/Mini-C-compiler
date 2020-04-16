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

  data TAC =                  AddCode Input Input Output      -- Add
                              | SubCode Input Input Output    -- Subtract
                              | MulCode Input Input Output    -- Multiply
                              | DivCode Input Input Output    -- Division
                              | EqlCode Input Input Output    -- Equal
                              | NqlCode Input Input Output    -- Not equal
                              | LssCode Input Input Output    -- Less than
                              | LeqCode Input Input Output    -- Less than / Equal
                              | GtrCode Input Input Output    -- Greater than
                              | GeqCode Input Input Output    -- Greater than / Equal
                              | AsnCode Input Output          -- Assign: should also have an output if the assignment itself produces a value
                              | MovCode Input Output          -- Move: basically same as Assign, except the move itself doesn't produce an output. Mostly used to move expressions into slots
                              | InvCode Input Output          -- Inverse
                              | IncCode Input Output          -- Increment
                              | DecCode Input Output          -- Decrement
                              | NotCode Input Output          -- Logical Not
                              | PuaCode Input                 -- Push argument
                              | LblCode Marker                -- Define label with marker
                              | JmpCode Marker                -- Unconditional jump
                              | JezCode Input Marker          -- Jump if input equals zero
                              | JnzCode Input Marker          -- Jump if input does not equal zero
                              | Rt0Code                       -- Return from function without value
                              | Rt1Code Input                 -- Return from function with value
                              | CllCode FunctionName Output   -- Call function
                              | PrtCode                       -- Call to print function
                              | BeginFunCode FunctionName
                              | EndFunCode FunctionName
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

  data TACFile              = TACFile { globalVarDeclarations :: GlobalVarTACs, functionDefinitions :: FunctionTACs }
                              deriving (Eq)

  instance Show TACFile where
    show (TACFile globals functions) =
      let globalString = concat  . intersperse "\n" $ map show globals
          functionsString = concat  . intersperse "\n" $ map show functions
      in "TACFile:\n\n" ++ globalString ++ "\n\n" ++ functionsString

  emptyTACFile :: TACFile
  emptyTACFile = TACFile [] []

  printFunName :: FunctionName
  printFunName = "print"

  readFunName :: FunctionName
  readFunName = "read"

  incGlobals :: Locations -> (TACLocation, Locations)
  incGlobals (Locations globals pars locals exps) = (Global globals, Locations (globals + 1) pars locals exps)
  incPars :: Locations -> (TACLocation, Locations)
  incPars (Locations globals pars locals exps) = (Parameter pars, Locations globals (pars + 1) locals exps)
  incLocals :: Locations -> (TACLocation, Locations)
  incLocals (Locations globals pars locals exps) = (Local locals, Locations globals pars (locals + 1) exps)
  incExps :: Locations -> (TACLocation, Locations)
  incExps (Locations globals pars locals exps) = (Local exps, Locations globals pars locals $ exps + 1)
  -- enterBlock :: Locations -> Locations
  -- enterBlock locs = Locations (globals locs) (pars locs) 0 0
  -- enterFun :: Locations -> Locations
  -- enterFun locs = Locations (globals locs) 0 0 0
  emptyLocations :: Locations
  emptyLocations = Locations 0 0 0 0

  newParEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  newParEnv env (Locations globals _ _ _) = (Environment.push env, Locations globals 0 0 0)

  newFunBodyEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  newFunBodyEnv env (Locations globals pars _ _) = (Environment.push env, Locations globals pars 0 0)

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
                                           in CompiledExp [tac] env locs binAddr

  unaryExpToTACs :: LocEnv -> Locations -> Expression -> UnOperator -> CompiledExp
  unaryExpToTACs env locs exp op = let CompiledExp leftTACs env1 locs1 argAddr = expToTACs env locs exp
                                       (outputAddr, locs2) = incExps locs1
                                       makeTAC = case op of
                                                 IncOp      -> IncCode
                                                 DecOp      -> DecCode
                                                 NotOp      -> NotCode
                                                 MinusUnOp  -> InvCode
                                       tac = makeTAC (InAddr argAddr) $ OutAddr outputAddr
                                   in CompiledExp [tac] env1 locs2 outputAddr


  compileArgs :: LocEnv -> Locations -> Expressions -> CompiledStm
  compileArgs env locs exps = let (env1, locs1, tacs1, inputAddresses) = foldl (\(env, locs, tacs, argInputAddresses) exp -> 
                                                                         let CompiledExp tacs1 env1 locs1 argInputAddr = expToTACs env locs exp
                                                                         in (env1, locs1, (tacs1 ++ tacs), (InAddr argInputAddr : argInputAddresses))) (env, locs, [], []) exps
                              in CompiledStm (tacs1 ++ (map PuaCode inputAddresses)) env1 locs1

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
  expToTACs env locs (BinaryExp op left right) = binaryExpToTACs env locs left right op
  expToTACs env locs (LeftExp (VariableRefExp name)) = let address = lookupInput name env
                                                           (outputAddr, locs1) = incExps locs
                                                           tac = MovCode (InAddr address) $ OutAddr outputAddr
                                                       in CompiledExp [tac] env locs1 outputAddr
  expToTACs env locs (UnaryExp op exp) = unaryExpToTACs env locs exp op
  expToTACs env locs (FunctionAppExp name args) = let CompiledStm evalArgsTACs env1 locs1 = compileArgs env locs args 
                                                      -- fnInput = InAddr $ lookupInput name env -- Should use the function's name instead of the address
                                                      (retAddress, locs2) = incExps locs1
                                                      callCode = if name == printFunName
                                                                 then PrtCode
                                                                 else CllCode name $ OutAddr retAddress
                                                      allTacs = evalArgsTACs `snoc` callCode
                                                  in CompiledExp allTacs env1 locs2 retAddress

  ifStmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  ifStmtToTacs env locs (IfStmt condExp thenBody m1 elseBody m2) =
    let lbl1 = LblCode m1
        lbl2 = LblCode m2
        (CompiledExp condExpTacs env1 locs1 condExpOutputAddr) = expToTACs env locs condExp
        condTacs = condExpTacs `snoc` JezCode (InAddr condExpOutputAddr) m1
        CompiledStm thenBodyTacs _ locs2 = bodyToTACs env1 locs1 thenBody -- Can ignore the env used in the body
        thenTacs = thenBodyTacs `snoc` JmpCode m2 `snoc` lbl1
        CompiledStm elseBodyTacs _ locs3 = bodyToTACs env1 locs2 elseBody -- Can ignore the env used in the body
        allTacs = condTacs ++ thenTacs ++ elseBodyTacs `snoc` lbl2
    in CompiledStm allTacs env locs3

  stmtToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  stmtToTacs env locs (ExpStmt exp) = let (CompiledExp tacs env' locs' _) = expToTACs env locs exp
                                      in CompiledStm tacs env' locs'
  stmtToTacs env locs (AssignStmt (VariableRefExp name) exp) = let (CompiledExp tacs env' locs' outputAddr) = expToTACs env locs exp
                                                                   toWrite = lookupInput name env
                                                                   input = InAddr outputAddr
                                                                   tac = AsnCode input $ OutAddr toWrite
                                                               in CompiledStm (tacs `snoc` tac) env' locs'
  stmtToTacs env locs ifStmt@(IfStmt _ _ _ _ _) = ifStmtToTacs env locs ifStmt
  stmtToTacs env locs Return0Stmt = CompiledStm [Rt0Code] env locs
  stmtToTacs env locs (Return1Stmt exp) = let (CompiledExp expTacs env' locs' outputAddr) = expToTACs env locs exp
                                              retTac = Rt1Code $ InAddr outputAddr
                                          in CompiledStm (expTacs `snoc` retTac) env' locs'
  stmtToTacs env locs _ = CompiledStm [] env locs

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
        addedEnv = Environment.insert name address env
        parsPushedEnv = Environment.push env
        (parsEnv, parsLocs) = foldl (\(env, locs) (VarDeclaration _ parName) -> 
                                       let (parAddr, locs1) = incPars locs
                                           parAddedEnv = Environment.insert parName parAddr env
                                       in (parAddedEnv, locs1)) (parsPushedEnv, updatedLocs) pars
        CompiledStm bodyTacs bodyEnv' bodyLocs' = bodyToTACs parsEnv parsLocs body
        fullFunctionTacs = (BeginFunCode name) : bodyTacs `snoc` EndFunCode name
    in CompiledStm fullFunctionTacs addedEnv updatedLocs -- Use addedEnv instead of bodyEnv', since we don't need the two pushed frames in the rest of the global scope

  -- generateTACs' :: LocEnv -> Locations -> Declarations -> (TACs, Locations)
  -- generateTACs' env locs [] = []
  -- generateTACs' env locs (decl:decls) = let CompiledStm tacs1 env1 locs1 = declarationToTACs env locs decl
  --                                           (tacs2, locs2) = generateTACs' env1 locs1 decls
  --                                       in (tacs1 ++ tacs2, locs2)

  defaultGlobalVarSize :: Integer
  defaultGlobalVarSize = 4

  generateTACs' :: LocEnv -> Locations -> Declarations -> TACFile -> TACFile
  generateTACs' env locs [] acc = acc
  generateTACs' env locs (decl@(VarDeclaration _ _):decls) (TACFile globalVars functions) =
    let globalVar = GlobalVarTAC (globals locs) defaultGlobalVarSize Nothing
        CompiledStm tacs1 env1 locs1 = declarationToTACs env locs decl
        newAcc = TACFile (globalVars `snoc` globalVar) functions
    in generateTACs' env1 locs1 decls newAcc
  generateTACs' env locs (decl@(FunDeclaration _ name _ _):decls) (TACFile globalVars functions)   =
    let CompiledStm tacs1 env1 locs1 = declarationToTACs env locs decl
        nrOfPars = pars locs1
        nrOfLocals = locals locs1
        nrOfExps = exps locs1
        functionTAC = FunctionTAC name nrOfPars nrOfLocals nrOfExps tacs1
        newAcc = TACFile globalVars (functions `snoc` functionTAC)
    in generateTACs' env1 locs1 decls newAcc

  generateTACs :: Declarations -> TACFile
  generateTACs decls = generateTACs' Environment.empty emptyLocations decls emptyTACFile

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