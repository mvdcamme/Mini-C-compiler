module CodeGeneration.MASM_86_Compile where

  import Data.Char
  import Data.List
  import Data.List.Extra

  import AST
  import CodeGeneration.MASM_86
  import CodeGeneration.MASM_86_Natives
  import ThreeAddressCode

  addressToLocalOffset :: Address -> Integer
  addressToLocalOffset address = (-1 - address) * 4

  idxToParOffset :: Integer -> Integer
  idxToParOffset idx = 8 + (idx * 4)

  inputToArg :: Input -> Arg
  inputToArg (ThreeAddressCode.Literal (IntValue i)) = MASM_86.Literal i
  inputToArg (ThreeAddressCode.Literal (CharValue c)) = MASM_86.Literal . toInteger $ ord c
  inputToArg (ThreeAddressCode.InAddr (ThreeAddressCode.Local address)) = Indirect (addressToLocalOffset address) BP
  inputToArg (ThreeAddressCode.InAddr (ThreeAddressCode.Parameter address)) = MASM_86.Parameter address

  outputToArg :: Output -> Arg
  outputToArg (ThreeAddressCode.OutAddr (Local address)) = Indirect (addressToLocalOffset address) BP
  outputToArg (ThreeAddressCode.OutAddr (ThreeAddressCode.Parameter address)) = MASM_86.Parameter address

  makeMovOp :: Arg -> Arg -> Operations
  -- Can't directly move between two memory locations
  makeMovOp dest@(Indirect _ _) source@(Indirect _ _) =
    [PushOp ax, MovOp ax source, MovOp dest ax, PopOp ax]
  makeMovOp arg1@(MASM_86.Parameter _) arg2@(MASM_86.Parameter _) =
    [PushOp ax, MovOp ax arg2, MovOp arg1 ax, PopOp ax]
  makeMovOp arg1@(Indirect _ _) arg2@(MASM_86.Parameter _) =
    [PushOp ax, MovOp ax arg2, MovOp arg1 ax, PopOp ax]
  makeMovOp arg2@(MASM_86.Parameter _) arg1@(Indirect _ _) =
    [PushOp ax, MovOp ax arg2, MovOp arg1 ax, PopOp ax]
  makeMovOp arg1 arg2 = [MovOp arg1 arg2]

  compileTAC :: TAC -> Operations
  compileTAC PrtCode = [CallOp "printInt"]
  compileTAC (MovCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in makeMovOp out' arg1
  compileTAC (AsnCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in makeMovOp out' arg1
  compileTAC (ArgCode in1) = [PushOp $ inputToArg in1]
  compileTAC Rt0Code =
    let functionExit = [MovOp sp bp, PopOp bp]
    in functionExit `snoc` RetOp
  compileTAC (Rt1Code in1) =
    let arg1 = inputToArg in1
        functionExit = [MovOp sp bp, PopOp bp]
    in makeMovOp retReg arg1 ++ functionExit `snoc` RetOp
  compileTAC ExtCode =
    [ MovOp ah (LiteralWithString "00h"),
      IntOp (LiteralWithString "16h"),
      MovOp ax (LiteralWithString "4C00h"),
      IntOp (LiteralWithString "21h")]
  compileTAC (CllCode name out) =
    let out' = outputToArg out
    in [CallOp name] ++ makeMovOp out' retReg
  compileTAC (AddCode in1 in2 out) = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
    in [PushOp ax] ++ makeMovOp ax arg1 ++ [AddOp ax arg2] ++ makeMovOp out' ax ++ [PopOp ax]
  compileTAC (SubCode in1 in2 out) = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
    in [PushOp ax] ++ makeMovOp ax arg1 ++ [SubOp ax arg2] ++ makeMovOp out' ax ++ [PopOp ax]

  compileFunTAC :: FunctionTAC -> Operations
  compileFunTAC (FunctionTAC name pars locals exps tacs) =
    let parsList = [0 .. pars - 1]
        -- makeArgOp = \i -> ArgOp i SizeDoubleWord
        -- argOps = map makeArgOp parsList
        -- functionPrelude = [StartprocOp name] ++ argOps
        functionPrelude = [StartprocOp name]
        functionEnter = [PushOp bp, MovOp bp sp, SubOp sp $ MASM_86.Literal (exps * 4)]
        mainSpecific = if (name == "main") then [StiOp, CldOp] else []
        functionExitlude = [EndprocOp name]
        functionTACs = foldl (\acc tac -> acc ++ (compileTAC tac)) [] tacs
    in functionPrelude ++ functionEnter ++ mainSpecific ++ functionTACs ++ functionExitlude

  regToString :: Register -> String
  regToString BP  = "bp"
  regToString IP  = "ip"
  regToString SP  = "sp"
  regToString SI  = "si"
  regToString DI  = "di"
  regToString AX  = "ax"
  regToString AH  = "ah"
  regToString AL  = "al"
  regToString BX  = "bx"
  regToString BH  = "bh"
  regToString BL  = "bl"
  regToString CX  = "cx"
  regToString CH  = "ch"
  regToString CL  = "cl"
  regToString DX  = "dx"
  regToString DH  = "dh"
  regToString DL  = "dl"

  -- nrToParName :: Integer -> VarName
  -- nrToParName i = "arg_" ++ show i

  -- parNameToString :: VarName -> String
  -- parNameToString name = "@@" ++ name

  -- parNrToString :: Integer -> String
  -- parNrToString i = parNameToString $ nrToParName i

  sizeEnumToString :: SizeEnum -> String
  sizeEnumToString SizeByte         = "byte"
  sizeEnumToString SizeWord         = "word"
  sizeEnumToString SizeDoubleWord   = "dword"

  argToString :: Arg -> String
  argToString (MASM_86.Literal i) = show i
  argToString (MASM_86.LiteralWithString s) = s
  argToString (Register reg) = regToString reg
  argToString (MASM_86.Parameter i) = argToString (Indirect (idxToParOffset i) BP)
  argToString (Indirect i reg) =
    let regName = regToString reg
    in if i < 0
       then "[" ++ regName ++ " - " ++ (show $ abs i) ++ "]"
       else "[" ++ regName ++ " + " ++ show i ++ "]"

  labelToString :: Label -> String
  labelToString (LabelString string) = string
  labelToString (LabelId i) = "label_" ++ show i

  modelSizeToString :: ModelSizeEnum -> String
  modelSizeToString ModelSizeSmall = "small"

  segmentToString :: Segment -> String
  segmentToString CodeSegment = ".code"
  segmentToString DataSegment = ".data"
  segmentToString StackSegment = ".stack"

  indentation :: Operation -> String
  indentation (EndprocOp _) = ""
  indentation (StartprocOp _) = ""
  indentation other = "\t"

  operationToString' :: Operation -> String
  operationToString' (MovOp arg1 arg2)     = "mov\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (AddOp arg1 arg2)     = "add\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (SubOp arg1 arg2)     = "sub\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' RetOp                 = "ret\t\t"
  operationToString' (IntOp arg1)          = "int\t\t" ++ argToString arg1
  operationToString' (PopOp arg1)          = "pop\t\t" ++ argToString arg1
  operationToString' (PushOp arg1)         = "push\t" ++ argToString arg1
  operationToString' (CallOp name)         = "call\t\t" ++ name
  -- operationToString' (ArgOp i size)        = "ARG\t" ++ parNrToString i ++ ": " ++ sizeEnumToString size
  operationToString' (StartprocOp name)    = name ++ " proc"
  operationToString' (EndprocOp name)      = name ++ " endp"
  operationToString' StiOp                 = "sti"
  operationToString' CldOp                 = "cld"
  operationToString' Empty                 = ""
  operationToString' (End name)            = "end " ++ name
  operationToString' (Model size)          = ".model " ++ modelSizeToString size
  operationToString' (StartSegment seg)    = segmentToString seg
  operationToString' (LblOp lbl1)          = labelToString lbl1 ++ ":"
  operationToString' (XorOp arg1 arg2)     = "xor\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (CmpOp arg1 arg2)     = "cmp\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (DivOp arg1)          = "div\t\t" ++ argToString arg1
  operationToString' (MASM_86.IncOp arg1)  = "inc\t\t" ++ argToString arg1
  operationToString' (MASM_86.DecOp arg1)  = "dec\t\t" ++ argToString arg1
  operationToString' (JnzOp lbl1)          = "jnz\t\t" ++ labelToString lbl1


  operationsToString :: Operations -> String
  operationsToString ops = concat (intersperse "\n" $ map (\tac -> (indentation tac) ++ (operationToString' tac)) ops)

  compile :: TACFile -> String
  compile (TACFile globals functions main) =
    let genericOps = concat $ map compileFunTAC functions
        mainOps = maybe [] compileFunTAC main
        ops = genericOps ++ mainOps
        body = operationsToString ops
        filePreludeString = operationsToString filePrelude
        fileExitludeString = operationsToString fileExitlude
    in filePreludeString ++ "\n\n" ++ body ++ "\n\n" ++ fileExitludeString