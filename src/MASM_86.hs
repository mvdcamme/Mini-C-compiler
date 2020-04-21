module MASM_86 where

  import Data.Char
  import Data.List
  import Data.List.Extra

  import AST
  import ThreeAddressCode

  data Register             = BP
                              | IP
                              | SP
                              | DI
                              | SI
                              -- General purpose registers
                              | AX
                              | AL
                              | AH
                              | BX
                              | BL
                              | BH
                              | CX
                              | CL
                              | CH
                              | DX
                              | DL
                              | DH
                              deriving (Show, Eq)

  data Arg                  = Literal Integer
                              | LiteralWithString String
                              | Register Register
                              | Parameter Integer
                              | Indirect Integer Register
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
                              | LabelId Integer
                              deriving (Show, Eq)

  data Operation            = MovOp Arg Arg
                              | PushOp Arg
                              | PopOp Arg
                              | CallOp FunctionName
                              | EndprocOp FunctionName
                              | StartprocOp FunctionName
                              | LblOp Label
                              | JmpOp Label
                              -- | ArgOp Integer SizeEnum -- discontinued
                              | AddOp Arg Arg
                              | SubOp Arg Arg
                              | XorOp Arg Arg
                              | CmpOp Arg Arg
                              | DivOp Arg
                              | IncOp Arg
                              | DecOp Arg
                              | JnzOp Label
                              | RetOp
                              | IntOp Arg -- Interrupt
                              | StiOp -- set The Interrupt Flag => enable interrupts
                              | CldOp -- clear The Direction Flag
                              -- Directives
                              | Model ModelSizeEnum
                              | StartSegment Segment
                              | End FunctionName
                              | Empty -- In case you want to add a newline, or a line consisting of only a comment
                              deriving (Show, Eq)
  type Operations           = [Operation]

  data FullOperation        = FullOp Operation (Maybe String)
                              deriving (Show, Eq)
  type FullOperations       = [FullOperation]

  bp :: Arg
  bp = Register BP
  sp :: Arg
  sp = Register SP

  ax :: Arg
  ax = Register AX
  ah :: Arg
  ah = Register AH
  al :: Arg
  al = Register AL
  bx :: Arg
  bx = Register BX
  bh :: Arg
  bh = Register BH
  bl :: Arg
  bl = Register BL
  cx :: Arg
  cx = Register CX
  ch :: Arg
  ch = Register CH
  cl :: Arg
  cl = Register CL
  dx :: Arg
  dx = Register DX
  dh :: Arg
  dh = Register DH
  dl :: Arg
  dl = Register DL

  retReg :: Arg -- The register where a function's return value will be placed in
  retReg = ax