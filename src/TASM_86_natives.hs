module TASM_86_Natives where

  import TASM_86

  filePrelude :: Operations
  filePrelude = 
    [
      Ideal,
      P386,
      ModelFlatC,
      Assume [
                Assumption CS Text,
                Assumption DS Flat,
                Assumption ES Flat,
                Assumption FS Flat,
                Assumption GS Flat
              ],
      StartSegment CodeSegment,
      StartprocOp "printInt",

      PushOp ebp SizeDoubleWord,
      MovOp ebp esp SizeDoubleWord,
      PushAd,
      MovOp eax (Indirect 8 EBP) SizeDoubleWord,
      MovOp ecx (Literal 0) SizeDoubleWord,
      MovOp ebx (Literal 10) SizeDoubleWord,

      LblOp $ LabelString "doOneDigit",
      XorOp edx edx,
      DivOp ebx,
      PushOp edx SizeDoubleWord,
      IncOp ecx,
      CmpOp eax $ Literal 0,
      JnzOp $ LabelString "doOneDigit",

      LblOp $ LabelString "printOneDigit",
      PopOp edx,
      AddOp edx $ Literal 48,
      MovOp ah (Literal 2) SizeDoubleWord,
      IntOp $ LiteralWithString "21h",
      DecOp ecx,
      JnzOp $ LabelString "printOneDigit",

      PopAd,
      MovOp esp ebp SizeDoubleWord,
      PopOp ebp,
      RetOp,

      EndprocOp "printInt"
    ]

--   filePrelude :: String
--   filePrelude  = 
--     "IDEAL\n\
-- \P386\n\
-- \MODEL FLAT, C\n\
-- \ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT\n\
-- \\n\
-- \CODESEG\n\
-- \\n\
-- \PROC printInt\n\
-- \\tpush    ebp\n\
-- \\tmov     ebp, esp\n\
-- \\tpushad\n\
-- \\n\
-- \\tmov     eax, [ebp+8]    ; retrieve argument 1\n\
-- \\n\
-- \\tmov     ecx, 0\n\
-- \\tmov     ebx, 10\n\
-- \ doOneDigit:\n\
-- \\txor     edx, edx        ; outcome is same as mov edx, 0\n\
-- \\tdiv     ebx\n\
-- \\tpush    edx             ; EDX contains one digit, push it to the stack\n\
-- \\tinc     ecx             ; count an extra digit\n\
-- \\tcmp     eax, 0          ; check if done, or more digits to process\n\
-- \\tjnz     doOneDigit\n\
-- \\n\
-- \printOneDigit:\n\
-- \\tpop     edx             ; retrieve a digit\n\
-- \\tadd     edx, 48         ; convert to ASCII character\n\
-- \\tmov     ah, 2           ; function 02h of int 21h\n\
-- \\tint     21h\n\
-- \\tdec     ecx             ; decrement the number of digits to print\n\
-- \\tjnz     printOneDigit\n\
-- \\n\
-- \\tpopad\n\
-- \\tmov     esp, ebp\n\
-- \\tpop     ebp\n\
-- \\tret\n\
-- \ENDP printInt\n\
-- \\n\
-- \"

  fileExitlude :: Operations
  fileExitlude =
    [
      StartSegment DataSegment,
      StartSegment StackSegment,
      End "main"
    ]

--   fileExitlude :: String
--   fileExitlude = 
--     "\n\n\
-- \; -------------------------------------------------------------------\n\
-- \; DATA\n\
-- \; -------------------------------------------------------------------\n\
-- \DATASEG\n\
-- \  msg db \"Hello World!\", 13, 10, '$'\n\
-- \\n\
-- \; -------------------------------------------------------------------\n\
-- \; STACK\n\
-- \; -------------------------------------------------------------------\n\
-- \STACK 100h\n\
-- \\n\
-- \END main\n\
-- \"