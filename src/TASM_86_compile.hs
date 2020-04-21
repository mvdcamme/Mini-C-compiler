module TASM_86_Compile where

  import Data.Char
  import Data.List
  import Data.List.Extra

  import AST
  import TASM_86
  import TASM_86_Natives
  import ThreeAddressCode

  addressToLocalOffset :: Address -> Integer
  addressToLocalOffset address = (-1 - address) * 4

  idxToParOffset :: Integer -> Integer
  idxToParOffset idx = 8 + (idx * 4)

  inputToArg :: Input -> Arg
  inputToArg (ThreeAddressCode.Literal (IntValue i)) = TASM_86.Literal i
  inputToArg (ThreeAddressCode.Literal (CharValue c)) = TASM_86.Literal . toInteger $ ord c
  inputToArg (ThreeAddressCode.InAddr (ThreeAddressCode.Local address)) = Indirect (addressToLocalOffset address) BP
  inputToArg (ThreeAddressCode.InAddr (ThreeAddressCode.Parameter address)) = TASM_86.Parameter address

  outputToArg :: Output -> Arg
  outputToArg (ThreeAddressCode.OutAddr (Local address)) = Indirect (addressToLocalOffset address) BP
  outputToArg (ThreeAddressCode.OutAddr (ThreeAddressCode.Parameter address)) = TASM_86.Parameter address

  makeMovOp :: Arg -> Arg -> SizeEnum -> Operations
  -- Can't directly move between two memory locations
  makeMovOp dest@(Indirect _ _) source@(Indirect _ _) size =
    [PushOp eax size, MovOp eax source size, MovOp dest eax size, PopOp eax]
  makeMovOp arg1@(TASM_86.Parameter _) arg2@(TASM_86.Parameter _) size =
    [PushOp eax size, MovOp eax arg2 size, MovOp arg1 eax size, PopOp eax]
  makeMovOp arg1@(Indirect _ _) arg2@(TASM_86.Parameter _) size =
    [PushOp eax size, MovOp eax arg2 size, MovOp arg1 eax size, PopOp eax]
  makeMovOp arg2@(TASM_86.Parameter _) arg1@(Indirect _ _) size =
    [PushOp eax size, MovOp eax arg2 size, MovOp arg1 eax size, PopOp eax]
  makeMovOp arg1 arg2 size = [MovOp arg1 arg2 size]

  compileTAC :: TAC -> Operations
  compileTAC (BeginFunCode name) = [] -- Code generated in compileFunTAC
  compileTAC (EndFunCode name) = []   -- Code generated in compileFunTAC
  compileTAC PrtCode = [CallOp "printInt"]
  compileTAC (MovCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in makeMovOp out' arg1 SizeDoubleWord
  compileTAC (AsnCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in makeMovOp out' arg1 SizeDoubleWord
  compileTAC (ArgCode in1) = [PushOp (inputToArg in1) SizeDoubleWord]
  compileTAC Rt0Code =
    let functionExit = [MovOp sp bp SizeDoubleWord, PopOp bp]
    in functionExit `snoc` RetOp
  compileTAC (Rt1Code in1) =
    let arg1 = inputToArg in1
        functionExit = [MovOp sp bp SizeDoubleWord, PopOp bp]
    in makeMovOp retReg arg1 SizeDoubleWord ++ functionExit `snoc` RetOp
  compileTAC ExtCode =
    [ MovOp ah (LiteralWithString "00h") SizeByte,
      IntOp (LiteralWithString "16h"),
      MovOp ax (LiteralWithString "4C00h") SizeWord,
      IntOp (LiteralWithString "21h")]
  compileTAC (CllCode name out) =
    let out' = outputToArg out
    in [CallOp name] ++ makeMovOp out' retReg SizeDoubleWord
  compileTAC (AddCode in1 in2 out) = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
    in [PushOp eax SizeDoubleWord] ++ makeMovOp eax arg1 SizeDoubleWord ++ [AddOp eax arg2] ++
       makeMovOp out' eax SizeDoubleWord ++ [PopOp eax]
  compileTAC (SubCode in1 in2 out) = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
    in [PushOp eax SizeDoubleWord] ++ makeMovOp eax arg1 SizeDoubleWord ++ [SubOp eax arg2] ++
       makeMovOp out' eax SizeDoubleWord ++ [PopOp eax]

  compileFunTAC :: FunctionTAC -> Operations
  compileFunTAC (FunctionTAC name pars locals exps tacs) =
    let parsList = [0 .. pars - 1]
        -- makeArgOp = \i -> ArgOp i SizeDoubleWord
        -- argOps = map makeArgOp parsList
        -- functionPrelude = [StartprocOp name] ++ argOps
        functionPrelude = [StartprocOp name]
        functionEnter = [PushOp bp SizeDoubleWord, MovOp bp sp SizeDoubleWord, SubOp sp $ TASM_86.Literal (exps * 4)]
        mainSpecific = if (name == "main") then [StiOp, CldOp] else []
        functionExitlude = [EndprocOp name]
        functionTACs = foldl (\acc tac -> acc ++ (compileTAC tac)) [] tacs
    in functionPrelude ++ functionEnter ++ mainSpecific ++ functionTACs ++ functionExitlude

  regToString :: Register -> String
  regToString BP  = "ebp"
  regToString EBP = "ebp"
  regToString IP  = "ip"
  regToString SP  = "esp"
  regToString ESP = "esp"
  regToString SI  = "si"
  regToString DI  = "di"
  regToString CS  = "cs"
  regToString DS  = "ds"
  regToString ES  = "es"
  regToString FS  = "fs"
  regToString GS  = "gs"
  regToString EAX = "eax"
  regToString AX  = "ax"
  regToString AH  = "ah"
  regToString AL  = "al"
  regToString EBX = "ebx"
  regToString BX  = "bx"
  regToString BH  = "bh"
  regToString BL  = "bl"
  regToString ECX = "ecx"
  regToString CX  = "cx"
  regToString CH  = "ch"
  regToString CL  = "cl"
  regToString EDX = "edx"
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
  argToString (TASM_86.Literal i) = show i
  argToString (TASM_86.LiteralWithString s) = s
  argToString (Register reg) = regToString reg
  argToString (TASM_86.Parameter i) = argToString (Indirect (idxToParOffset i) BP)
  argToString (Indirect i reg) =
    let regName = regToString reg
    in if i < 0
       then "[ dword " ++ regName ++ " - " ++ (show $ abs i) ++ "]"
       else "[ dword " ++ regName ++ " + " ++ show i ++ "]"

  labelToString :: Label -> String
  labelToString (LabelString string) = string
  labelToString (LabelId i) = "label_" ++ show i

  modelSizeToString :: ModelSizeEnum -> String
  modelSizeToString ModelSizeSmall = "small"

  segmentToString :: Segment -> String
  segmentToString CodeSegment = "CODESEG"
  segmentToString DataSegment = "DATASEG"
  segmentToString StackSegment = "STACK"

  assumptionToString :: Assumption -> String
  assumptionToString (Assumption reg Flat) = regToString reg ++ ":FLAT"
  assumptionToString (Assumption reg Text) = regToString reg ++ ":_TEXT"

  assumptionsToString :: [Assumption] -> String
  assumptionsToString assumptions = intercalate "," $ map assumptionToString assumptions

  indentation :: Operation -> String
  indentation (EndprocOp _) = ""
  indentation (StartprocOp _) = ""
  indentation other = "\t"

  operationToString' :: Operation -> String
  operationToString' (MovOp arg1 arg2 size)       = "mov\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (AddOp arg1 arg2)            = "add\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (SubOp arg1 arg2)            = "sub\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' RetOp                        = "ret\t\t"
  operationToString' (IntOp arg1)                 = "int\t\t" ++ argToString arg1
  operationToString' (PopOp arg1)                 = "pop\t\t" ++ argToString arg1
  operationToString' (PushOp arg1 size)           = "push\t" ++ argToString arg1
  operationToString' (CallOp name)                = "call\t\t" ++ name
  -- operationToString' (ArgOp i size)            = "ARG\t" ++ parNrToString i ++ ": " ++ sizeEnumToString size
  operationToString' (StartprocOp name)           = "PROC " ++ name
  operationToString' (EndprocOp name)             = "ENDP " ++ name
  operationToString' StiOp                        = "sti"
  operationToString' CldOp                        = "cld"
  operationToString' PopAd                        = "popad"
  operationToString' PushAd                       = "pushad"
  operationToString' Ideal                        = "IDEAL"
  operationToString' P386                         = "P386"
  operationToString' ModelFlatC                   = "MODEL FLAT, C"
  operationToString' Empty                        = ""
  operationToString' (End name)                   = "end " ++ name
  operationToString' (Assume assumptions)         = "ASSUME " ++ assumptionsToString assumptions
  operationToString' (StartSegment seg)           = segmentToString seg
  operationToString' (LblOp lbl1)                 = labelToString lbl1 ++ ":"
  operationToString' (XorOp arg1 arg2)            = "xor\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (CmpOp arg1 arg2)            = "cmp\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (DivOp arg1)                 = "div\t\t" ++ argToString arg1
  operationToString' (TASM_86.IncOp arg1)         = "inc\t\t" ++ argToString arg1
  operationToString' (TASM_86.DecOp arg1)         = "dec\t\t" ++ argToString arg1
  operationToString' (JnzOp lbl1)                 = "jnz\t\t" ++ labelToString lbl1

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

  -- compile :: TACFile -> String
  -- compile (TACFile globals functions) = show . compileFunTAC $ head function'"s