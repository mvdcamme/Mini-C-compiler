module TASM_86_Compile where

  import Control.Monad
  import Control.Monad.State
  import Data.Char
  import Data.List
  import Data.List.Extra
  import Data.Map hiding (map, foldl) 
  import Debug.Trace

  import AST
  import TASM_86
  import TASM_86_Natives
  import TASM_86_Definitions
  import ThreeAddressCode as TAC
  import Type

  data CompilerStateState   = CompilerStateState { cssPars :: Integer
                                                 , cssLocals :: Integer }
                              deriving (Show, Eq)
  type CompilerState a      = State CompilerStateState a

  conditionalLabelName :: String
  conditionalLabelName = "label"

  addressToLocalOffset :: Address -> Integer
  addressToLocalOffset address = (-1 - address) * dwordSize

  idxToParOffset :: Integer -> Integer
  idxToParOffset idx = (2 * dwordSize) + (idx * dwordSize)

  inputToArg :: Input -> TASM_86_Compiled Arg
  inputToArg (TAC.Literal (IntValue i)) = return $ TASM_86.Literal i
  inputToArg (TAC.Literal (CharValue c)) = return . TASM_86.Literal . toInteger $ ord c
  inputToArg (TAC.InAddr loc) =
    do memRef <- locToMemoryRef loc
       return $ Memory memRef -- (trace ("called locToMemoryRef in inputToArg with result " ++ (show memRef)) memRef)

  outputToArg :: Output -> TASM_86_Compiled Arg
  outputToArg (TAC.OutAddr loc) =
    do memRef <- locToMemoryRef loc
       return $ Memory memRef -- (trace ("called locToMemoryRef in outputToArg with result " ++ (show memRef)) memRef)

  clearArg :: Arg -> Operations
  clearArg TASM_86.Literal{} = []
  clearArg TASM_86.LiteralWithString{} = []
  clearArg mem@Memory{} = [MovOp mem $ TASM_86.Literal 0] -- add size
  clearArg reg@Register{} = [XorOp reg reg]
  clearArg reg@DerefRegister{} = [MovOp reg $ TASM_86.Literal 0] -- add size

  -- castMemoryRef :: (MemoryReference, SizeEnum) -> (MemoryReference, SizeEnum) -> Operations
  -- castMemoryRef (inMem@(Memory _), inSize) (outMem@(Memory _), outSize) =
  --   let inEaxOfSize = Register $ getRegOfSize EAX inSize
  --       outEaxOfSize = Register $ getRegOfSize EAX outSize
  --   in saving [EAX] [MovOp inEaxOfSize inMem, MovOp outMem outEaxOfSize]

  castRegister :: (Register, SizeEnum) -> (Register, SizeEnum) -> Operations
  castRegister (inReg, _) (outReg, _) | 
    (regVariantToGeneralReg inReg) == (regVariantToGeneralReg outReg) = []
  castRegister (inReg, inSize) (outReg, outSize) =
    (clearArg $ Register outReg) `snoc` MovOp (Register outReg) (Register inReg)

  doCstCode :: (Arg, SizeEnum) -> (Arg, SizeEnum) -> Operations
  doCstCode (inArg@(Memory _), inSize) (out@(Memory _), outSize) =
    let inEaxOfSize = Register $ getRegOfSize EAX inSize
        outEaxOfSize = Register $ getRegOfSize EAX outSize
    in saving [EAX] [XorOp eax eax, MovOp inEaxOfSize inArg, MovOp out outEaxOfSize]
  doCstCode (Register inReg, inSize) (Register outReg, outSize) =
    castRegister (inReg, inSize) (outReg, outSize)
  -- doCstCode (inArg, _) (outArg, _) = [MovOp outArg inArg]

  -- castToHigher :: Register -> SizeEnum -> (Register, Operations)
  -- castToHigher reg size =
  --   let newReg = getRegOfSize reg size
  --       clear = clearArg (Register newReg)
  --   in

  -- castInputToOutput :: Input -> Output -> [Operations]
  -- castInputToOutput TAC.Literal{} _ = [] -- Shouldn't happen?
  -- castInputToOutput (TAC.InAddr inLoc) (TAC.OutAddr outLoc) =
  --   let inSize = typeToSize $ getType inLoc
  --       outSize = typeToSize $ getType outLoc

  saving :: [Register] -> Operations -> Operations
  saving regs ops =
    let pushes = map (\reg -> PushOp $ Register reg) regs
        pops = reverse $ map (\reg -> PopOp $ Register reg) regs
    in pushes ++ ops ++ pops

  makeIndirectOp :: Arg -> Arg -> SizeEnum -> (Arg -> Arg -> Operation) -> Operations
  makeIndirectOp arg1@(Memory _) arg2@(Memory _) size create =
    let eaxOfSize = getRegOfSize EAX size
        eaxOfSize' = Register eaxOfSize
    in saving [eaxOfSize] [MovOp eaxOfSize' arg2, create arg1 eaxOfSize']
  makeIndirectOp arg1 arg2 size create = [create arg1 arg2]

  -- Can't directly move between two memory locations
  makeMovOp :: Arg -> Arg -> SizeEnum -> Operations
  makeMovOp dest source size = makeIndirectOp dest source size MovOp

  makePrefixOp :: Input -> Output -> (Arg -> Operation) -> TASM_86_Compiled Operations
  makePrefixOp in1 out create = 
    do arg1 <- inputToArg in1
       out' <- outputToArg out
       let size = typeToSize $ getType out
       return $ [create arg1] ++ makeMovOp out' arg1 size

  makeSuffixOp :: Input -> Output -> (Arg -> Operation) -> TASM_86_Compiled Operations
  makeSuffixOp in1 out create =
    do arg1 <- inputToArg in1
       out' <- outputToArg out
       let size = typeToSize $ getType out
       return $ (makeMovOp out' arg1 size) ++ [create arg1]

  makeConditionalJump :: Input -> Marker -> (Label -> Operation) -> TASM_86_Compiled Operations
  makeConditionalJump in1 (Marker loc) create =
    do arg1 <- inputToArg in1
       return [CmpOp arg1 $ TASM_86.Literal 0, create $ LabelId conditionalLabelName loc]

  makeComparisonLabel :: Integer -> Label
  makeComparisonLabel id = LabelId "cmp" id

  makeComparisonExp :: Input -> Input -> Output -> (Label -> Operation) -> TASM_86_Compiled ()
  makeComparisonExp in1 in2 out makeJmp = 
    do arg1 <- inputToArg in1
       arg2 <- inputToArg in2
       out' <- outputToArg out
       let size = typeToSize $ getType out
       id1 <- newLabel
       let label1 = makeComparisonLabel id1
       id2 <- newLabel
       let label2 = makeComparisonLabel id2
       -- Compare in1 with in2
       fromOps $ makeIndirectOp arg1 arg2 size (\a b -> CmpOp a b)
       -- Do the conditional jump
       fromOps [makeJmp label1]
       -- Comparison was false: move 0 to out
       fromOps [MovOp out' $ TASM_86.Literal 0, JmpOp label2]
       -- Comparison was true: move 1 to out
       fromOps [LblOp label1, MovOp out' $ TASM_86.Literal 1, LblOp label2]

  makeRemoveParsOp :: Integer -> Operations
  makeRemoveParsOp 0 = []
  makeRemoveParsOp n =
    let argsSize = n * dwordSize
    in [AddOp sp $ TASM_86.Literal argsSize]


  -- al & bl -> ax ?
  -- ax & bx -> eax ?
  -- eax & ebx -> eax + edx ?
  doMulCode :: Arg -> Arg -> Arg -> SizeEnum -> TASM_86_Compiled ()
  doMulCode in1 in2 out' size =
    let eaxOfSize = Register $ getRegOfSize EAX size
        ebxOfSize = Register $ getRegOfSize EBX size
        movIn1 = makeMovOp eaxOfSize in1 size
        movIn2 = makeMovOp ebxOfSize in2 size
        movOut = makeMovOp out' eaxOfSize size
    in fromOps . saving [EAX, EBX] $ (movIn1 ++ movIn2 ++ [MulOp ebxOfSize] ++ movOut)

  compileTAC :: TAC -> TASM_86_Compiled ()
  -- Arithmetic
  compileTAC (AddCode in1 in2 out) = 
    do arg1 <- inputToArg in1
       arg2 <- inputToArg in2
       out' <- outputToArg out
       let size = typeToSize $ getType out
       let eaxOfSize = getRegOfSize EAX size
       let movIn1 = makeMovOp (Register eaxOfSize) arg1 size
       let movOut = makeMovOp out' (Register eaxOfSize) size
       let ops = saving [eaxOfSize] (movIn1 ++ [AddOp (Register eaxOfSize) arg2] ++ movOut)
       fromOps ops
  compileTAC (SubCode in1 in2 out) = 
    do arg1 <- inputToArg in1
       arg2 <- inputToArg in2
       out' <- outputToArg out
       let size = typeToSize $ getType out
       let eaxOfSize = getRegOfSize EAX size
       let movIn1 = makeMovOp (Register eaxOfSize) arg1 size
       let movOut = makeMovOp out' (Register eaxOfSize) size
       let ops = saving [eaxOfSize] (movIn1 ++ [SubOp (Register eaxOfSize) arg2] ++ movOut)
       fromOps ops
  compileTAC (MulCode in1 in2 out) =
    do arg1 <- inputToArg in1
       arg2 <- inputToArg in2
       out' <- outputToArg out
       let size = typeToSize $ getType out
       doMulCode arg1 arg2 out' size
  compileTAC (PrefixIncCode in1 out) = do ops <- makePrefixOp in1 out IncOp; fromOps ops
  compileTAC (SuffixIncCode in1 out) = do ops <- makeSuffixOp in1 out IncOp; fromOps ops
  compileTAC (PrefixDecCode in1 out) = do ops <- makePrefixOp in1 out DecOp; fromOps ops
  compileTAC (SuffixDecCode in1 out) = do ops <- makeSuffixOp in1 out DecOp; fromOps ops
  -- Comparison
  compileTAC (EqlCode in1 in2 out) = makeComparisonExp in1 in2 out JeOp
  compileTAC (NqlCode in1 in2 out) = makeComparisonExp in1 in2 out JneOp
  compileTAC (GtrCode in1 in2 out) = makeComparisonExp in1 in2 out JgOp
  compileTAC (GeqCode in1 in2 out) = makeComparisonExp in1 in2 out JgeOp
  compileTAC (LssCode in1 in2 out) = makeComparisonExp in1 in2 out JlOp
  compileTAC (LeqCode in1 in2 out) = makeComparisonExp in1 in2 out JleOp
  -- Labels and jumps
  compileTAC (LblCode (Marker loc)) = (fromOps . LblOp $ LabelId conditionalLabelName loc)
  compileTAC (JmpCode (Marker loc)) = (fromOps . JmpOp $ LabelId conditionalLabelName loc)
  compileTAC (JnzCode in1 marker) =
    do ops <- makeConditionalJump in1 marker JnzOp
       fromOps ops
  compileTAC (JzCode in1 marker) =
    do ops <- makeConditionalJump in1 marker JzOp
       fromOps ops
  -- -- Function calls and returns
  compileTAC (ArgCode in1) =
    do arg1 <- inputToArg in1
       fromOps $ PushOp arg1
  compileTAC (CllCode name n out) =
     do out' <- outputToArg out
        let size = typeToSize $ getType out
        fromOps $ CallOp name
        fromOps $ makeRemoveParsOp n
        fromOps $ makeMovOp out' retReg size
  compileTAC (PrtCode n) = (fromOps $ CallOp "printInt") >> (fromOps $ makeRemoveParsOp n)
  compileTAC Rt0Code =
    let functionExitOps = [MovOp sp bp, PopOp bp]
    in fromOps functionExitOps >> fromOps RetOp
  compileTAC (Rt1Code in1) =
    do arg1 <- inputToArg in1
       let functionExitOps = [MovOp sp bp, PopOp bp]
       let size = typeToSize $ getType in1
       fromOps $ clearArg retReg
       fromOps $ makeMovOp (retRegOfSize size) arg1 size
       fromOps functionExitOps
       fromOps RetOp
  -- Assignments
  compileTAC (AsnCode in1 (OutAddr (TACLocationPointsTo out))) =
    do arg1 <- inputToArg in1
       out' <- outputToArg $ OutAddr out
       let inSize = typeToSize $ getType in1
       let outSize = typeToSize $ getType out
       let eaxOfSize = getRegOfSize EAX outSize
       let ebxOfSize = getRegOfSize EBX inSize
       fromOps $ saving [eaxOfSize, ebxOfSize]
                        ((XorOp ebx ebx) : 
                         [MovOp (Register eaxOfSize) out', MovOp (Register ebxOfSize) arg1,
                          MovOp (DerefRegister eaxOfSize outSize) . Register $ getRegOfSize EBX outSize])
  compileTAC (AsnCode in1 out) =
    do arg1 <- inputToArg in1
       out' <- outputToArg out
       let size = typeToSize $ getType out
       fromOps $ makeMovOp out' arg1 size
  -- Casting
  compileTAC (CstCode in1 (OutAddr (TACLocationPointsTo out))) =
    do arg1 <- inputToArg in1
       out' <- outputToArg $ OutAddr out
       let size = typeToSize $ getType out
       let eaxOfSize = getRegOfSize EAX size
       let ebxOfSize = getRegOfSize EBX size
       fromOps $ saving [eaxOfSize, ebxOfSize]
                        [MovOp (Register eaxOfSize) out', MovOp (Register ebxOfSize) arg1,
                         MovOp (DerefRegister eaxOfSize size) $ Register ebxOfSize]
  compileTAC (CstCode in1 out) =
    do arg1 <- inputToArg in1
       out' <- outputToArg out
       let inSize = typeToSize $ getType in1
       let outSize = typeToSize $ getType out
       fromOps $ doCstCode (arg1, inSize) (out', outSize)
  -- Pointers and addresses
  compileTAC (AdrCode (InAddr (TACLocationPointsTo in1)) out) =
    do arg1 <- inputToArg $ InAddr in1
       out' <- outputToArg out
       let size = typeToSize $ getType out
       let eaxOfSize = getRegOfSize EAX size
       fromOps $ saving [eaxOfSize] [LeaOp (Register eaxOfSize) arg1, MovOp out' $ Register eaxOfSize]
  compileTAC (DrfCode in1 out) =
    do arg1 <- inputToArg in1
       out' <- outputToArg out
       let size = typeToSize $ getType out
       let eaxOfSize = getRegOfSize EAX size
       let ebxOfSize = getRegOfSize EBX size
       fromOps $ saving [eaxOfSize, ebxOfSize] 
                        [MovOp eax arg1,
                         MovOp (Register ebxOfSize) $ DerefRegister EAX size,
                         MovOp out' $ Register ebxOfSize]
  compileTAC (ParCode in1 in2 out) =
    do arg1 <- inputToArg in1
       arg2 <- inputToArg in2
       out' <- outputToArg out
       let in2Type = getType in1
       let typeSize = typeToInteger in2Type
       fromOps $ saving [EAX, EBX]
                        ((XorOp eax eax) :
                         (makeMovOp ax arg1 SizeWord) ++ (makeMovOp bx (TASM_86.Literal typeSize) SizeWord) ++
                         ([MulOp bx, AddOp eax arg2] ++ (makeMovOp out' eax SizeDoubleWord)))

  -- Machine operations
  compileTAC (MovCode in1 out) =
    do arg1 <- inputToArg in1
       out' <- outputToArg out
       let size = typeToSize $ getType out
       fromOps $ makeMovOp out' arg1 size
  compileTAC ExtCode =
    fromOps [-- MovOp ah (LiteralWithString "00h") SizeByte,
             -- IntOp (LiteralWithString "16h"),
             MovOp ax $ LiteralWithString "4C00h",
             IntOp $ LiteralWithString "21h"]
  -- compileTAC code = trace ("Unsupported: " ++ show code) (return ())

  calculateFrameSize :: LocalAddresses -> Integer
  calculateFrameSize localAddresses =
    let sizes = Data.Map.fold (\typ acc -> typeToInteger typ : acc) [] localAddresses
        ebpSize = dwordSize
        defaultFrameContents = [ebpSize]
    in (foldl (+) 0 sizes) + (foldl (+) 0 defaultFrameContents)

  compileFunTAC :: FunctionTAC -> TASM_86_Compiled ()
  compileFunTAC (FunctionTAC name pars locals exps tacs localAddresses) =
    let parsList = [0 .. pars - 1]
        frameSize = calculateFrameSize localAddresses
        functionPreludeOps = [StartprocOp name]
        functionEnterOps = [PushOp bp, MovOp bp sp, SubOp sp $ TASM_86.Literal frameSize]
        mainSpecificOps = if (name == "main") then [StiOp, CldOp] else []
        functionExitludeOps = [EndprocOp name]
    in do fromOps functionPreludeOps
          fromOps functionEnterOps
          fromOps mainSpecificOps
          mapM (\tac -> fromOps (Comment $ show tac) >> compileTAC tac) tacs
          fromOps functionExitludeOps

  compileGlobalVarTAC :: GlobalVarTAC -> GlobalVarDefinition
  compileGlobalVarTAC (GlobalVarTAC addr size value) =
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
  argToString (Memory (TASM_86.Parameter i size)) = argToString (Memory $ Indirect (idxToParOffset i) EBP size)
  argToString (Memory (Indirect i reg size)) =
    let regName = regToString reg
    in if i < 0
       then "[ " ++ (sizeEnumToString size) ++ " " ++ regName ++ " - " ++ (show $ abs i) ++ " ]"
       else "[ " ++ (sizeEnumToString size) ++ " " ++ regName ++ " + " ++ show i ++ " ]"
  argToString (Memory (TASM_86.Global addr)) = "[ offset " ++ globalAddressToName addr ++ " ]"
  argToString (DerefRegister reg size) =
    "[ " ++ sizeEnumToString size ++ " " ++ regToString reg ++ " ]"

  labelToString :: Label -> String
  labelToString (LabelString string) = string
  labelToString (LabelId name i) = name ++ "_" ++ show i

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
  operationToString' (MulOp arg1)                 = "mul\t\t" ++ argToString arg1
  operationToString' (CallOp name)                = "call\t" ++ name
  operationToString' RetOp                        = "ret\t\t"
  operationToString' (MovOp arg1 arg2)            = "mov\t\t" ++ argToString arg1 ++ ", " ++ argToString arg2
  operationToString' (IntOp arg1)                 = "int\t\t" ++ argToString arg1
  operationToString' (PopOp arg1)                 = "pop\t\t" ++ argToString arg1
  operationToString' (PushOp arg1)                = "push\t" ++ argToString arg1
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
  operationToString' (TASM_86.Comment comment)       = "; " ++ comment

  operationsToString :: Operations -> String
  operationsToString ops = concat (intersperse "\n" $ map (\tac -> (indentation tac) ++ (operationToString' tac)) ops)

  globalVarDefinitionToString :: GlobalVarDefinition -> String
  globalVarDefinitionToString (GlobalVarDefinition name size Nothing) = 
    let sizeString = case size of {
                      SizeByte -> "db";
                      SizeWord -> "dw";
                      SizeDoubleWord -> "dw"
    }
    in "\t" ++ name ++ " " ++ sizeString ++ " ?"

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
    let (_, state) = newCompiled (do mapM (\funTAC -> compileFunTAC funTAC) functions
                                     maybe (return ()) (\v -> compileFunTAC v) main)
        globalVarDefinitions = map compileGlobalVarTAC globals
        file = CompiledFile filePrelude globalVarDefinitions $ tasmOps state
    in compiledFileToString file