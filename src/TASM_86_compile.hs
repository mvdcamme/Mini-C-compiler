module TASM_86_Compile where

  import Data.Char
  import Data.List
  import Data.List.Extra

  import AST
  import TASM_86
  import TASM_86_Natives
  import TASM_86_Definitions
  import ThreeAddressCode as TAC

  dwordSize :: Integer
  dwordSize = 4

  addressToLocalOffset :: Address -> Integer
  addressToLocalOffset address = (-1 - address) * dwordSize

  idxToParOffset :: Integer -> Integer
  idxToParOffset idx = (2 * dwordSize) + (idx * dwordSize)

  inputToArg :: Input -> Arg
  inputToArg (TAC.Literal (IntValue i)) = TASM_86.Literal i
  inputToArg (TAC.Literal (CharValue c)) = TASM_86.Literal . toInteger $ ord c
  inputToArg (TAC.InAddr (TAC.Local address)) = Memory $ Indirect (addressToLocalOffset address) BP
  inputToArg (TAC.InAddr (TAC.Parameter address)) = Memory $ TASM_86.Parameter address
  inputToArg (TAC.InAddr (TAC.Global address)) = Memory $ TASM_86.Global address
  -- inputToArg (TAC.InAddr (TAC.TACLocationPointsTo address)) = Memory $ TASM_86.Global address

  outputToArg :: Output -> Arg
  outputToArg (TAC.OutAddr (Local address)) = Memory $ Indirect (addressToLocalOffset address) BP
  outputToArg (TAC.OutAddr (TAC.Parameter address)) = Memory $ TASM_86.Parameter address
  outputToArg (TAC.OutAddr (TAC.Global address)) = Memory $ TASM_86.Global address
  -- outputToArg (TAC.OutAddr (TACLocationPointsTo tacLoc)) = Memory $ TASM_86.Global 99

  saving :: [Register] -> Operations -> Operations
  saving regs ops =
    let pushes = map (\reg -> PushOp (Register reg) $ regToSize reg) regs
        pops = map (\reg -> PopOp $ Register reg) regs
    in pushes ++ ops ++ pops

  makeIndirectOp :: Arg -> Arg -> SizeEnum -> (Arg -> Arg -> SizeEnum -> Operation) -> Operations
  makeIndirectOp arg1@(Memory _) arg2@(Memory _) size create =
    saving [EAX] [MovOp eax arg2 size, create arg1 eax size]
  makeIndirectOp arg1 arg2 size create = [create arg1 arg2 size]

  -- Can't directly move between two memory locations
  makeMovOp :: Arg -> Arg -> SizeEnum -> Operations
  makeMovOp dest source size = makeIndirectOp dest source size MovOp
  -- makeMovOp dest@(Memory _) source@(Memory _) size =
  --   saving [EAX] [MovOp eax source size, MovOp dest eax size]
  -- makeMovOp dest source size = [MovOp dest source size]

  makePrefixOp :: Input -> Output -> (Arg -> Operation) -> Operations
  makePrefixOp in1 out create = 
    let arg1 = inputToArg in1
        out' = outputToArg out
    in [create arg1] ++ makeMovOp out' arg1 SizeDoubleWord

  makeSuffixOp :: Input -> Output -> (Arg -> Operation) -> Operations
  makeSuffixOp in1 out create =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in (makeMovOp out' arg1 SizeDoubleWord) ++ [create arg1]

  makeConditionalJump :: Input -> Marker -> (Label -> Operation) -> Operations
  makeConditionalJump in1 (Marker loc) create =
    let arg1 = inputToArg in1
    in [CmpOp arg1 $ TASM_86.Literal 0, create $ LabelId loc]

  makeComparisonExp :: Input -> Input -> Output -> (Label -> Operation) -> TASM_86_Compiled ()
  makeComparisonExp in1 in2 out makeJmp = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
    in do id1 <- newLabel
          let label1 = LabelString ("cmp_" ++ show id1)
          id2 <- newLabel
          let label2 = LabelString ("cmp_" ++ show id2)
          -- Compare in1 with in2
          fromOps $ makeIndirectOp arg1 arg2 SizeDoubleWord (\a b _ -> CmpOp a b)
          -- Do the conditional jump
          fromOps [makeJmp label1]
          -- Comparison was false: move 0 to out
          fromOps [MovOp out' (TASM_86.Literal 0) SizeDoubleWord, JmpOp label2]
          -- Comparison was true: move 1 to out
          fromOps [LblOp label1, MovOp out' (TASM_86.Literal 1) SizeDoubleWord, LblOp label2]

  makeRemoveParsOp :: Integer -> Operations
  makeRemoveParsOp 0 = []
  makeRemoveParsOp n =
    let argsSize = n * dwordSize
    in [SubOp sp $ TASM_86.Literal argsSize]

  compileTAC :: TAC -> TASM_86_Compiled ()
  -- Arithmetic
  compileTAC (AddCode in1 in2 out) = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
        movIn1 = makeMovOp eax arg1 SizeDoubleWord
        movOut = makeMovOp out' eax SizeDoubleWord
        ops = saving [EAX] (movIn1 ++ [AddOp eax arg2] ++ movOut)
    in fromOps ops
  compileTAC (SubCode in1 in2 out) = 
    let arg1 = inputToArg in1
        arg2 = inputToArg in2
        out' = outputToArg out
        movIn1 = makeMovOp eax arg1 SizeDoubleWord
        movOut = makeMovOp out' eax SizeDoubleWord
        ops = saving [EAX] (movIn1 ++ [SubOp eax arg2] ++ movOut)
    in fromOps ops
  compileTAC (PrefixIncCode in1 out) = (fromOps $ makePrefixOp in1 out IncOp)
  compileTAC (SuffixIncCode in1 out) = (fromOps $ makeSuffixOp in1 out IncOp)
  compileTAC (PrefixDecCode in1 out) = (fromOps $ makePrefixOp in1 out DecOp)
  compileTAC (SuffixDecCode in1 out) = (fromOps $ makeSuffixOp in1 out DecOp)
  -- Comparison
  compileTAC (EqlCode in1 in2 out) = makeComparisonExp in1 in2 out JeOp
  compileTAC (NqlCode in1 in2 out) = makeComparisonExp in1 in2 out JneOp
  compileTAC (GtrCode in1 in2 out) = makeComparisonExp in1 in2 out JgOp
  compileTAC (GeqCode in1 in2 out) = makeComparisonExp in1 in2 out JgeOp
  compileTAC (LssCode in1 in2 out) = makeComparisonExp in1 in2 out JlOp
  compileTAC (LeqCode in1 in2 out) = makeComparisonExp in1 in2 out JleOp
  -- Labels and jumps
  compileTAC (LblCode (Marker loc)) = (fromOps . LblOp $ LabelId loc)
  compileTAC (JmpCode (Marker loc)) = (fromOps . JmpOp $ LabelId loc)
  compileTAC (JnzCode in1 marker) = (fromOps $ makeConditionalJump in1 marker JnzOp)
  compileTAC (JzCode in1 marker) = (fromOps $ makeConditionalJump in1 marker JzOp  )
  -- -- Function calls and returns
  compileTAC (ArgCode in1) = (fromOps $ PushOp (inputToArg in1) SizeDoubleWord)
  compileTAC (CllCode name n out) =
    let out' = outputToArg out
    in do fromOps $ CallOp name
          fromOps $ makeRemoveParsOp n
          fromOps $ makeMovOp out' retReg SizeDoubleWord
  compileTAC (PrtCode n) = (fromOps $ CallOp "printInt") >> (fromOps $ makeRemoveParsOp n)
  compileTAC Rt0Code =
    let functionExitOps = [MovOp sp bp SizeDoubleWord, PopOp bp]
    in fromOps functionExitOps >> fromOps RetOp
  compileTAC (Rt1Code in1) =
    let arg1 = inputToArg in1
        functionExitOps = [MovOp sp bp SizeDoubleWord, PopOp bp]
    in do fromOps $ makeMovOp retReg arg1 SizeDoubleWord
          fromOps functionExitOps
          fromOps RetOp
  -- Assignments
  compileTAC (AsnCode in1 (OutAddr (TACLocationPointsTo out))) =
    let arg1 = inputToArg in1
        out' = outputToArg $ OutAddr out
        size = SizeDoubleWord
    in (fromOps $ saving [EAX, EBX]
                         [MovOp eax out' size, MovOp ebx arg1 size,
                          MovOp (DerefRegister EAX) ebx size])
  compileTAC (AsnCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in fromOps $ makeMovOp out' arg1 SizeDoubleWord
  -- Pointers and addresses
  compileTAC (AdrCode (InAddr (TACLocationPointsTo in1)) out) =
    let arg1 = inputToArg $ InAddr in1
        out' = outputToArg out
    in fromOps $ saving [EAX] [LeaOp eax arg1, MovOp out' eax SizeDoubleWord]
  compileTAC (DrfCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
        size = SizeDoubleWord
    in fromOps $ saving [EAX, EBX] 
                        [MovOp eax arg1 size,
                         MovOp ebx (DerefRegister EAX) size,
                         MovOp out' ebx size]
  -- Machine operations
  compileTAC (MovCode in1 out) =
    let arg1 = inputToArg in1
        out' = outputToArg out
    in fromOps $ makeMovOp out' arg1 SizeDoubleWord
  compileTAC ExtCode =
    fromOps [MovOp ah (LiteralWithString "00h") SizeByte,
             IntOp (LiteralWithString "16h"),
             MovOp ax (LiteralWithString "4C00h") SizeWord,
             IntOp (LiteralWithString "21h")]

  compileFunTAC :: FunctionTAC -> TASM_86_Compiled ()
  compileFunTAC (FunctionTAC name pars locals exps tacs) =
    let parsList = [0 .. pars - 1]
        -- makeArgOp = \i -> ArgOp i SizeDoubleWord
        -- argOps = map makeArgOp parsList
        -- functionPrelude = [StartprocOp name] ++ argOps
        functionPreludeOps = [StartprocOp name]
        functionEnterOps = [PushOp bp SizeDoubleWord, MovOp bp sp SizeDoubleWord, SubOp sp $ TASM_86.Literal (exps * dwordSize)]
        mainSpecificOps = if (name == "main") then [StiOp, CldOp] else []
        functionExitludeOps = [EndprocOp name]
    in do fromOps functionPreludeOps
          fromOps functionEnterOps
          fromOps mainSpecificOps
          foldl (\acc tac -> acc >> compileTAC tac) (return ()) tacs
          fromOps functionExitludeOps

  compileGlobalVarTAC :: GlobalVarTAC -> GlobalVarDefinition
  compileGlobalVarTAC (GlobalVarTAC addr 4 value) =
    GlobalVarDefinition (globalAddressToName addr) SizeDoubleWord value

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

  globalAddressToName :: Integer -> String
  globalAddressToName addr = "global_" ++ show addr

  argToString :: Arg -> String
  argToString (TASM_86.Literal i) = show i
  argToString (TASM_86.LiteralWithString s) = s
  argToString (Register reg) = regToString reg
  argToString (Memory (TASM_86.Parameter i)) = argToString (Memory $ Indirect (idxToParOffset i) BP)
  argToString (Memory (Indirect i reg)) =
    let regName = regToString reg
    in if i < 0
       then "[ dword " ++ regName ++ " - " ++ (show $ abs i) ++ " ]"
       else "[ dword " ++ regName ++ " + " ++ show i ++ " ]"
  argToString (Memory (TASM_86.Global addr)) = "[ offset " ++ globalAddressToName addr ++ " ]"
  argToString (DerefRegister reg) =
    "[ dword " ++ regToString reg ++ " ]"

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
  indentation End{} = ""
  indentation EndprocOp{} = ""
  indentation StartprocOp{} = ""
  indentation Ideal = ""
  indentation P386 = ""
  indentation ModelFlatC = ""
  indentation Assume{} = ""
  indentation Empty = ""
  indentation LblOp{} = "\t"
  indentation other = "\t\t"

  -- concatString :: [String] -> String
  -- concatString [] = ""
  -- concatString str:[] = str
  -- concatString str1:str2:[] = 

  operationToString' :: Operation -> String
  operationToString' (AddOp arg1 arg2)            = "add\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (SubOp arg1 arg2)            = "sub\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (CallOp name)                = "call\t" ++ name
  operationToString' RetOp                        = "ret\t\t"
  operationToString' (MovOp arg1 arg2 size)       = "mov\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (IntOp arg1)                 = "int\t\t" ++ argToString arg1
  operationToString' (PopOp arg1)                 = "pop\t\t" ++ argToString arg1
  operationToString' (PushOp arg1 size)           = "push\t" ++ argToString arg1
  operationToString' (LeaOp arg1 arg2)            = "lea\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
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
  operationToString' (LblOp lbl1)                 = labelToString lbl1 ++ ":"
  operationToString' (JmpOp label)                = "jmp\t\t" ++ labelToString label
  operationToString' (JnzOp label)                = "jnz\t\t" ++ labelToString label
  operationToString' (JzOp label)                 = "jz\t\t" ++ labelToString label
  operationToString' (JeOp label)                 = "je\t\t" ++ labelToString label
  operationToString' (JneOp label)                = "jne\t\t" ++ labelToString label
  operationToString' (JgOp label)                 = "jg\t\t" ++ labelToString label
  operationToString' (JgeOp label)                = "jge\t\t" ++ labelToString label
  operationToString' (JlOp label)                 = "jl\t\t" ++ labelToString label
  operationToString' (JleOp label)                = "jle\t\t" ++ labelToString label
  operationToString' (End name)                   = "end " ++ name
  operationToString' (Assume assumptions)         = "ASSUME " ++ assumptionsToString assumptions
  operationToString' (StartSegment seg)           = segmentToString seg
  operationToString' (XorOp arg1 arg2)            = "xor\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (CmpOp arg1 arg2)            = "cmp\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (DivOp arg1)                 = "div\t\t" ++ argToString arg1
  operationToString' (TASM_86.IncOp arg1)         = "inc\t\t" ++ argToString arg1
  operationToString' (TASM_86.DecOp arg1)         = "dec\t\t" ++ argToString arg1

  operationsToString :: Operations -> String
  operationsToString ops = concat (intersperse "\n" $ map (\tac -> (indentation tac) ++ (operationToString' tac)) ops)

  globalVarDefinitionToString :: GlobalVarDefinition -> String
  globalVarDefinitionToString (GlobalVarDefinition name SizeDoubleWord Nothing) = "\t" ++ name ++ " dw ?"

  globalVarDefinitionsToString :: GlobalVarDefinitions -> String
  globalVarDefinitionsToString defs = concat (intersperse "\n" $ map globalVarDefinitionToString defs)

  data CompiledFile = CompiledFile
    { prefix :: Operations
    , dataSegment :: GlobalVarDefinitions
    , codeSegment :: Operations }

  compiledFileToString :: CompiledFile -> String
  compiledFileToString file =
    let filePreludeString = operationsToString $ prefix file
        fileExitludeString = "\n\n" ++ operationsToString fileExitlude
        globalVarDefinitionsString = globalVarDefinitionsToString $ dataSegment file
        nativeFunctionsString = operationsToString nativeFunctions
        userFunctionsStrings = operationsToString $ codeSegment file
        stackSegmentString =
          "\n\nSTACK\n\n"
        dataSegmentString =
          "\n\nDATASEG\n\n" ++ globalVarDefinitionsString
        codeSegmentString =
          "\n\nCODESEG\n\n" ++ nativeFunctionsString ++ "\n\n" ++ userFunctionsStrings
    in filePreludeString ++ stackSegmentString ++
       dataSegmentString ++ codeSegmentString ++
       fileExitludeString

  compile :: TACFile -> String
  compile (TACFile globals functions main) =
    let (_, (id, ops)) = newCompiled (do foldl (\acc funTAC -> acc >> compileFunTAC funTAC) (return ()) functions
                                         maybe (return ()) (\v -> compileFunTAC v) main)
        globalVarDefinitions = map compileGlobalVarTAC globals
        file = CompiledFile filePrelude globalVarDefinitions ops
    in compiledFileToString file
    -- let 
    --     (TASM_86_Compiled _ ops _) = maybe genericOps (\v -> compileFunTAC genericOps v) main
    --     globalVarDefinitions = map compileGlobalVarTAC globals
    --     file = CompiledFile filePrelude globalVarDefinitions ops
    -- in compiledFileToString file

  -- compile :: TACFile -> String
  -- compile (TACFile globals functions) = show . compileFunTAC $ head function'"s