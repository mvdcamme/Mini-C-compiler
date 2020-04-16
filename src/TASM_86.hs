module TASM_86 where

    type FunctionName =       String

    data Register =           BP
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

    data Arg =                Literal Integer
                              | Register Register
                              | Indirect Integer Register
                              deriving (Show, Eq)

    data Operation =          Mov Arg Arg
                              | Push Arg
                              | Call FunctionName
                              | Endproc
                              | Startproc
                              | Add Arg Arg
                              | Sub Arg Arg
                              deriving (Show, Eq)

    filePrelude :: String
    filePrelude  = 
      ".section  __TEXT,__text,regular,pure_instructions\n\
      \.build_version macos, 10, 15  sdk_version 10, 15\n\
      \.globl  _main\n\
      \.p2align  4, 0x90"