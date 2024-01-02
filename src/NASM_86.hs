module NASM_86 where

  import Data.Char
  import Data.List
  import Data.List.Extra

  import AST
  import ThreeAddressCode
  import Type

  data Register             = BP
                              | EBP
                              | IP
                              | SP
                              | ESP
                              | DI
                              | SI
                              | CS
                              | DS
                              | ES
                              | FS
                              | GS
                              -- General purpose registers
                              | EAX
                              | AX
                              | AL
                              | AH
                              | EBX
                              | BX
                              | BL
                              | BH
                              | ECX
                              | CX
                              | CL
                              | CH
                              | EDX
                              | DX
                              | DL
                              | DH
                              deriving (Show, Eq)

  data MemoryReference      = Parameter Address SizeEnum
                              | Indirect Address Register SizeEnum
                              | Global Integer SizeEnum
                              deriving (Show, Eq)

  data Arg                  =  Literal Integer
                              | LiteralWithString String
                              | Register Register
                              | Memory MemoryReference
                              | DerefRegister Register SizeEnum
                              | AssemblyGlobalVariable String
                              deriving (Show, Eq)

  data SizeEnum             = SizeByte
                              | SizeWord
                              | SizeDoubleWord
                              deriving (Show, Eq)

  data ModelSizeEnum        = ModelSizeSmall
                              deriving (Show, Eq)

  data Segment              = CodeSegment
                              | DataSegment
                              | StackSegment
                              deriving (Show, Eq)

  data Label                = LabelString String
                              | LabelId String Integer
                              deriving (Show, Eq)

  data Style                = Flat
                              | Text
                              deriving (Show, Eq)
  data Assumption           = Assumption Register Style
                              deriving (Show, Eq)

  data Operation            = MovOp Arg Arg
                              | PushOp Arg
                              | PopOp Arg
                              | CallOp FunctionName
                              | EndprocOp FunctionName
                              | StartprocOp FunctionName
                              | LblOp Label
                              | JmpOp Label
                              | JeOp Label
                              | JneOp Label
                              | JgOp Label
                              | JgeOp Label
                              | JlOp Label
                              | JleOp Label
                              | JzOp Label
                              | JnzOp Label
                              | AddOp Arg Arg
                              | SubOp Arg Arg
                              | MulOp Arg
                              | XorOp Arg Arg
                              | CmpOp Arg Arg
                              | DivOp Arg
                              | IncOp Arg
                              | DecOp Arg
                              | LeaOp Arg Arg
                              | RetOp
                              | IntOp Arg -- Interrupt
                              | StiOp -- set The Interrupt Flag => enable interrupts
                              | CldOp -- clear The Direction Flag
                              | PopAd
                              | PushAd

                              | LiteralString String String
                              -- Directives
                              | GlobalFunctionDeclaration String
                              | ExternFunctionDeclaration String
                              | StartSegment Segment
                              | End FunctionName
                              | Empty -- In case you want to add a newline, or a line consisting of only a comment
                              | Comment String
                              deriving (Show, Eq)
  type Operations           = [Operation]

  data GlobalVarDefinition  = GlobalVarDefinition VarName SizeEnum (Maybe Integer)
  type GlobalVarDefinitions = [GlobalVarDefinition]

  bp :: Arg
  bp = Register BP
  ebp :: Arg
  ebp = Register EBP
  sp :: Arg
  sp = Register SP
  esp :: Arg
  esp = Register ESP

  eax :: Arg
  eax = Register EAX
  ax :: Arg
  ax = Register AX
  ah :: Arg
  ah = Register AH
  al :: Arg
  al = Register AL
  ebx :: Arg
  ebx = Register EBX
  bx :: Arg
  bx = Register BX
  bh :: Arg
  bh = Register BH
  bl :: Arg
  bl = Register BL
  ecx :: Arg
  ecx = Register ECX
  cx :: Arg
  cx = Register CX
  ch :: Arg
  ch = Register CH
  cl :: Arg
  cl = Register CL
  edx :: Arg
  edx = Register EDX
  dx :: Arg
  dx = Register DX
  dh :: Arg
  dh = Register DH
  dl :: Arg
  dl = Register DL

  regToSize :: Register -> SizeEnum
  regToSize EAX = SizeDoubleWord
  regToSize EBX = SizeDoubleWord
  regToSize ECX = SizeDoubleWord
  regToSize EDX = SizeDoubleWord
  regToSize AX  = SizeWord
  regToSize BX  = SizeWord
  regToSize CX  = SizeWord
  regToSize DX  = SizeWord
  regToSize AH  = SizeByte
  regToSize AL  = SizeByte
  regToSize BH  = SizeByte
  regToSize BL  = SizeByte
  regToSize CH  = SizeByte
  regToSize CL  = SizeByte
  regToSize DH  = SizeByte
  regToSize DL  = SizeByte

  retReg' :: Register
  retReg' = EAX

  retReg :: Arg -- The register where a function's return value will be placed in
  retReg = Register retReg'

  retRegOfSize :: SizeEnum -> Arg
  retRegOfSize size = Register $ getRegOfSize retReg' size

  typeToSize :: Type -> SizeEnum
  typeToSize (Atom VoidType) = SizeByte       -- TODO: Shouldn't happen
  typeToSize (ArrayType _ _) = SizeDoubleWord
  typeToSize (PointerType _) = SizeDoubleWord
  typeToSize (Atom CharType) = SizeWord
  typeToSize (Atom IntType)  = SizeDoubleWord

  byteSize :: Integer
  byteSize = 1
  wordSize :: Integer
  wordSize = 2
  dwordSize :: Integer
  dwordSize = 4

  sizeToInteger :: SizeEnum -> Integer
  sizeToInteger SizeByte = byteSize
  sizeToInteger SizeWord = wordSize
  sizeToInteger SizeDoubleWord = dwordSize

  typeToInteger :: Type -> Integer
  typeToInteger (ArrayType size typ) = size * typeToInteger typ
  typeToInteger typ = sizeToInteger $ typeToSize typ

  regVariantToGeneralReg :: Register -> Register
  regVariantToGeneralReg AL  = EAX
  regVariantToGeneralReg AH  = EAX
  regVariantToGeneralReg AX  = EAX
  regVariantToGeneralReg EAX = EAX
  regVariantToGeneralReg BL  = EBX
  regVariantToGeneralReg BH  = EBX
  regVariantToGeneralReg BX  = EBX
  regVariantToGeneralReg EBX = EBX
  regVariantToGeneralReg CL  = ECX
  regVariantToGeneralReg CH  = ECX
  regVariantToGeneralReg CX  = ECX
  regVariantToGeneralReg ECX = ECX
  regVariantToGeneralReg DL  = EDX
  regVariantToGeneralReg DH  = EDX
  regVariantToGeneralReg DX  = EDX
  regVariantToGeneralReg EDX = EDX

  getRegOfSize' :: Register -> SizeEnum -> Register
  getRegOfSize' EAX SizeByte        = AL
  getRegOfSize' EBX SizeByte        = BL
  getRegOfSize' ECX SizeByte        = CL
  getRegOfSize' EDX SizeByte        = DL
  getRegOfSize' EAX SizeWord        = AX
  getRegOfSize' EBX SizeWord        = BX
  getRegOfSize' ECX SizeWord        = CX
  getRegOfSize' EDX SizeWord        = DX
  getRegOfSize' EAX SizeDoubleWord  = EAX
  getRegOfSize' EBX SizeDoubleWord  = EBX
  getRegOfSize' ECX SizeDoubleWord  = ECX
  getRegOfSize' EDX SizeDoubleWord  = EDX

  getRegOfSize :: Register -> SizeEnum -> Register
  getRegOfSize reg size = getRegOfSize' (regVariantToGeneralReg reg) size

