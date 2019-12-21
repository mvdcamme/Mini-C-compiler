module ThreeAddressCode where

  import AST
  import Environment
  import Type

  type Address =              Integer

  data AtomicValue =          IntValue Integer
                              | CharValue Char

  data Input =                Literal AtomicValue
                              | InAddr Address
  data Output =               OutAddr Address

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
                              | AsnCode Input Output          -- Assign
                              | InvCode Input Output          -- Inverse
                              | IncCode Input Output          -- Increment
                              | DecCode Input Output          -- Decrement
                              | NotCode Input Output          -- Logical Not
                              | PushArgCode Input             -- Push argument
                              | ParCode Input Output
                              | LocalVarCode Input Output
                              | GlobalVarCode Input Output
                              | BeginFunCode
                              | EndFunCode

  type TACs =                 [TAC]

  data Location =             Global Address
                              | Local Address
                              | Parameter Address

  data Locations =            Locations { globals :: Address, pars :: Address, locals :: Address, exps :: Address }

  type LocEnv =               GenericEnvironment ThreeAddressCode.Location

  incGlobals :: Locations -> (Address, Locations)
  incGlobals (Locations globals pars locals exps) = (globals, Locations (globals + 1) pars locals exps)
  incPars :: Locations -> (Address, Locations)
  incPars (Locations globals pars locals exps) = (pars, Locations globals (pars + 1) locals exps)
  incLocals :: Locations -> (Address, Locations)
  incLocals (Locations globals pars locals exps) = (locals, Locations globals pars (locals + 1) exps)
  incExps :: Locations -> (Address, Locations)
  incExps (Locations globals pars locals exps) = (locals, Locations globals pars locals $ exps + 1)
  enterFun :: Locations -> Locations
  enterFun locs = Locations (globals locs) 0 0 0
  emptyLocals :: Locations
  emptyLocals = Locations 0 0 0 0

  data Tuple4 = Tuple4 { tacs :: TACs, env :: LocEnv, locs :: Locations, input :: Input }
  data InputTuple4 = InputTuple4 { iptacs :: TACs, ipenv :: LocEnv, iplocs :: Locations, ipinputs :: [Input] }

  binaryExpToTACs :: LocEnv -> Locations -> Expression -> Expression -> BinOperator -> (TACs, LocEnv, Locations, Input)
  binaryExpToTACs env locs left right op = let (leftTACs, env1, locs1, input1) = expToTACs env locs left
                                               (rightTACs, env2, locs2, input2) = expToTACs env1 locs1 right
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
                                           in ([tac], env, locs, InAddr output)

  unaryExpToTACs :: LocEnv -> Locations -> Expression -> UnOperator -> (TACs, LocEnv, Locations, Input)
  unaryExpToTACs env locs exp op = let (leftTACs, env1, locs1, input1) = expToTACs env locs exp
                                       (address, locs2) = incExps locs1
                                       makeTAC = case op of
                                                 IncOp      -> IncCode
                                                 DecOp      -> DecCode
                                                 NotOp      -> NotCode
                                                 MinusUnOp  -> InvCode
                                       tac = makeTAC input1 $ OutAddr address
                                   in ([tac], env1, locs2, InAddr address)

  -- expressions should always produce some input (or is it an output?)
  expToTACs :: LocEnv -> Locations -> Expression -> (TACs, LocEnv, Locations, Input)
  expToTACs env locs (NumberExp integer) = ([], env, locs, Literal $ IntValue integer)
  expToTACs env locs (QCharExp char) = ([], env, locs, Literal $ CharValue char)
  expToTACs env locs (BinaryExp op left right) = binaryExpToTACs env locs left right op
  expToTACs env locs (LeftExp (VariableRefExp name)) = let (address, locs1) = incLocals locs
                                                       in case Environment.lookup name env of
                                                          Just (Global global) -> ([GlobalVarCode (InAddr global) (OutAddr address)], env, locs1, InAddr address)
                                                          Just (Local local) -> ([LocalVarCode (InAddr local) (OutAddr address)], env, locs1, InAddr address)
                                                          Just (Parameter par) -> ([ParCode (InAddr par) (OutAddr address)], env, locs1, InAddr address)
                                                          Nothing -> ([], env, locs, InAddr (-1)) -- TODO: Should produce an error
  expToTACs env locs (UnaryExp op exp) = unaryExpToTACs env locs exp op
  expToTACs env locs (FunctionAppExp name args) = let (InputTuple4 evalArgsTACs env1 locs1 inputs) = foldl (\ip arg ->
                                                                                                            let (tacs2, env2, locs2, input2) = expToTACs (ipenv ip) (iplocs ip) arg
                                                                                                            in InputTuple4 (iptacs ip ++ tacs2) env2 locs2 (ipinputs ip ++ [input2]) 
                                                                                                           ) first args
                                                                                            where first = InputTuple4 [] env locs []
                                                      (address, locs3) = incExps locs1
                                                  in ((evalArgsTACs ++ map (\(input) -> PushArgCode input) inputs), env1, locs3, InAddr address) -- TODO Add CallCode

  declarationToTACs :: LocEnv -> Locations -> Declaration -> (TACs, LocEnv, Locations)
  declarationToTACs env locs (VarDeclaration _ name) = let (address, updatedLocs) = incLocals locs
                                                           updatedEnv = Environment.insert name (Global address) env
                                                       in ([GlobalVarCode (Literal $ IntValue 0) $ OutAddr address], updatedEnv, updatedLocs)

  generateTACs' :: LocEnv -> Locations -> Declarations -> TACs
  generateTACs' env locs [] = []
  generateTACs' env locs (decl:decls) = let (tacs, env1, locs1) = declarationToTACs env locs decl
                                        in tacs ++ generateTACs' env1 locs1 decls

  generateTACs :: Declarations -> TACs
  generateTACs tacs = generateTACs' Environment.empty emptyLocals tacs