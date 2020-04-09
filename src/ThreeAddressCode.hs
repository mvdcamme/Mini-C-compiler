module ThreeAddressCode where

  import AST
  import Environment
  import Type

  type Address =              Integer

  data AtomicValue =          IntValue Integer
                              | CharValue Char
                              deriving (Show, Eq)

  data Input =                Literal AtomicValue
                              | InAddr Address
                              deriving (Show, Eq)
  data Output =               OutAddr Address
                              deriving (Show, Eq)

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
                              | AsnCode Input Input           -- Assign: should also have an output if the assignment itself produces a value
                              | InvCode Input Output          -- Inverse
                              | IncCode Input Output          -- Increment
                              | DecCode Input Output          -- Decrement
                              | NotCode Input Output          -- Logical Not
                              | PuaCode Input                 -- Push argument
                              | CllCode Input Output          -- Call function
                              | PrtCode Input                 -- Call to print function
                              | BeginFunCode String
                              | EndFunCode String
                              deriving (Show, Eq)

  type TACs =                 [TAC]

  data Location =             Global Address
                              | Local Address
                              | Parameter Address
                              deriving (Show, Eq)

  data Locations =            Locations { globals :: Address, pars :: Address, locals :: Address, exps :: Address }

  type LocEnv =               GenericEnvironment ThreeAddressCode.Location

  data CompiledExp = CompiledExp { expTacs :: TACs, expEnv :: LocEnv, expLocs :: Locations, input :: Input }
  data CompiledStm = CompiledStm { stmTacs :: TACs, stmEnv :: LocEnv, stmLocs :: Locations }
  data InputTuple4 = InputTuple4 { iptacs :: TACs, ipenv :: LocEnv, iplocs :: Locations, ipinputs :: [Input] }

  printFunName :: String
  printFunName = "print"

  incGlobals :: Locations -> (Address, Locations)
  incGlobals (Locations globals pars locals exps) = (globals, Locations (globals + 1) pars locals exps)
  incPars :: Locations -> (Address, Locations)
  incPars (Locations globals pars locals exps) = (pars, Locations globals (pars + 1) locals exps)
  incLocals :: Locations -> (Address, Locations)
  incLocals (Locations globals pars locals exps) = (locals, Locations globals pars (locals + 1) exps)
  incExps :: Locations -> (Address, Locations)
  incExps (Locations globals pars locals exps) = (locals, Locations globals pars locals $ exps + 1)
  -- enterBlock :: Locations -> Locations
  -- enterBlock locs = Locations (globals locs) (pars locs) 0 0
  enterFun :: Locations -> Locations
  enterFun locs = Locations (globals locs) 0 0 0
  emptyLocations :: Locations
  emptyLocations = Locations 0 0 0 0

  newParEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  newParEnv env (Locations globals _ _ _) = (Environment.push env, Locations globals 0 0 0)

  newFunBodyEnv :: LocEnv -> Locations -> (LocEnv, Locations)
  newFunBodyEnv env (Locations globals pars _ _) = (Environment.push env, Locations globals pars 0 0)




  binaryExpToTACs :: LocEnv -> Locations -> Expression -> Expression -> BinOperator -> CompiledExp
  binaryExpToTACs env locs left right op = let CompiledExp leftTACs env1 locs1 input1 = expToTACs env locs left
                                               CompiledExp rightTACs env2 locs2 input2 = expToTACs env1 locs1 right
                                               (output, locs3) = incExps locs2
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
                                               tac = makeTAC input1 input2 $ OutAddr output
                                           in CompiledExp [tac] env locs $ InAddr output

  unaryExpToTACs :: LocEnv -> Locations -> Expression -> UnOperator -> CompiledExp
  unaryExpToTACs env locs exp op = let CompiledExp leftTACs env1 locs1 input1 = expToTACs env locs exp
                                       (address, locs2) = incExps locs1
                                       makeTAC = case op of
                                                 IncOp      -> IncCode
                                                 DecOp      -> DecCode
                                                 NotOp      -> NotCode
                                                 MinusUnOp  -> InvCode
                                       tac = makeTAC input1 $ OutAddr address
                                   in CompiledExp [tac] env1 locs2 $ InAddr address


  compileArgs :: LocEnv -> Locations -> Expressions -> CompiledStm
  compileArgs env locs exps = let (env1, locs1, tacs1, inputs1) = foldl (\(env, locs, tacs, inputs) exp -> 
                                                                         let CompiledExp tacs1 env1 locs1 input = expToTACs env locs exp
                                                                         in (env1, locs1, (tacs1 ++ tacs), (input : inputs))) (env, locs, [], []) exps
                              in CompiledStm (tacs1 ++ (map PuaCode inputs1)) env1 locs1

  lookupInput :: Name -> LocEnv -> Input -- TODO should produce an error if name cannot be found
  lookupInput name env = let address = case Environment.lookup name env of
                                       Just (Global global) -> global
                                       Just (Local local) -> local
                                       Just (Parameter par) -> par
                                       _ -> (-1)
                         in InAddr address

  -- expressions should always produce some input
  expToTACs :: LocEnv -> Locations -> Expression -> CompiledExp
  expToTACs env locs (NumberExp integer) = CompiledExp [] env locs . Literal $ IntValue integer
  expToTACs env locs (QCharExp char) = CompiledExp [] env locs . Literal $ CharValue char
  expToTACs env locs (BinaryExp op left right) = binaryExpToTACs env locs left right op
  expToTACs env locs (LeftExp (VariableRefExp name)) = let input = lookupInput name env
                                                       in CompiledExp [] env locs input
  expToTACs env locs (UnaryExp op exp) = unaryExpToTACs env locs exp op
  expToTACs env locs (FunctionAppExp name args) = let CompiledStm evalArgsTACs env1 locs1 = compileArgs env locs args 
                                                      fnInput = lookupInput name env
                                                      (retAddress, locs2) = incExps locs1
                                                      callCode = if name == printFunName
                                                                 then PrtCode fnInput
                                                                 else CllCode fnInput $ OutAddr retAddress
                                                      allTacs = evalArgsTACs ++ [callCode]
                                                  in CompiledExp allTacs env1 locs2 $ InAddr retAddress

  stmToTacs :: LocEnv -> Locations -> Statement -> CompiledStm
  stmToTacs env locs (ExpStmt exp) = let (CompiledExp tacs env' locs' _) = expToTACs env locs exp
                                     in CompiledStm tacs env' locs'
  stmToTacs env locs (AssignStmt (VariableRefExp name) exp) = let (CompiledExp tacs env' locs' input) = expToTACs env locs exp
                                                                  toWrite = lookupInput name env
                                                                  tac = AsnCode input toWrite
                                                              in CompiledStm (tacs ++ [tac]) env' locs'
  stmToTacs env locs _ = CompiledStm [] env locs

  addDeclarations :: LocEnv -> Locations -> (Locations -> (Address, Locations)) -> (Address -> ThreeAddressCode.Location) -> Declarations -> (LocEnv, Locations)
  addDeclarations env locs inc toLoc decls =
    foldl (\(env, locs) name ->
            let (address, locs1) = inc locs
                env1 = Environment.insert name (toLoc address) env
            in (env1, locs1))
          (env, locs) $ map (\(VarDeclaration _ name) -> name) decls

  globalVarDefToTACs :: LocEnv -> Locations -> Declaration -> CompiledStm
  globalVarDefToTACs env locs (VarDeclaration _ name) = let (address, updatedLocs) = incGlobals locs
                                                            updatedEnv = Environment.insert name (Global address) env
                                                        in CompiledStm [] updatedEnv updatedLocs -- No TACs needed

  declarationToTACs :: LocEnv -> Locations -> Declaration -> CompiledStm
  declarationToTACs env locs decl@(VarDeclaration _ _) = globalVarDefToTACs env locs decl
  declarationToTACs env locs (FunDeclaration _ name pars body) = 
    let (address, updatedLocs) = incGlobals locs
        addedEnv = Environment.insert name (Global address) env
        parsPushedEnv = Environment.push env
        (parsEnv, parsLocs) = foldl (\(env, locs) (VarDeclaration _ parName) -> 
                                       let (parAddr, locs1) = incPars locs
                                           parAddedEnv = Environment.insert parName (Parameter parAddr) env
                                       in (parAddedEnv, locs1)) (parsPushedEnv, updatedLocs) pars
        bodyPushedEnv = Environment.push parsEnv
        Body bodyDecls bodyStmts = body
        (bodyEnv, bodyLocs) = addDeclarations bodyPushedEnv parsLocs incLocals Local bodyDecls
        (bodyEnv', bodyLocs', bodyTacs) = foldl (\(env, locs, tacs) stm -> 
          let CompiledStm tacs1 env1 locs1 = stmToTacs env locs stm
          in (env1, locs1, tacs ++ tacs1 ++ [BeginFunCode "XXX"])) (bodyEnv, bodyLocs, []) bodyStmts
        fullFunctionTacs = (BeginFunCode name) : bodyTacs ++ [EndFunCode name]
    in CompiledStm fullFunctionTacs addedEnv bodyLocs' -- Use addedEnv instead of bodyEnv', since we don't need the two pushed frames in the rest of the global scope

  generateTACs' :: LocEnv -> Locations -> Declarations -> TACs
  generateTACs' env locs [] = []
  generateTACs' env locs (decl:decls) = let CompiledStm tacs env1 locs1 = declarationToTACs env locs decl
                                        in tacs ++ generateTACs' env1 locs1 decls

  generateTACs :: Declarations -> TACs
  generateTACs tacs = generateTACs' Environment.empty emptyLocations tacs