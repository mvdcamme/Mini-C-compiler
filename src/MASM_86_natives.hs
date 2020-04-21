module MASM_86_Natives where

  import MASM_86

  filePrelude :: Operations
  filePrelude =
    [
      Empty,
      Model ModelSizeSmall,
      StartSegment StackSegment,
      StartSegment DataSegment,
      StartSegment CodeSegment,

      StartprocOp "printInt",
      PushOp bp,
      MovOp bp sp,
      MovOp ax $ Indirect 4 BP,
      MovOp cx $ Literal 0,
      MovOp bx $ Literal 10,

      LblOp $ LabelString "doOneDigit",
      XorOp dx dx,
      DivOp bx,
      PushOp dx,
      IncOp cx,
      CmpOp ax $ Literal 0,
      JnzOp $ LabelString "doOneDigit",

      LblOp $ LabelString "printOneDigit",
      PopOp dx,
      AddOp dx $ Literal 48,
      MovOp ah $ Literal 2,
      IntOp $ LiteralWithString "21h",
      DecOp cx,
      JnzOp $ LabelString "printOneDigit",

      MovOp sp bp,
      PopOp bp,
      RetOp,

      EndprocOp "printInt"
    ]

  fileExitlude :: Operations
  fileExitlude =
    [
      End "main"
    ]