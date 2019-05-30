module ThreeAddressCode where

  import Type

  type Address =  Int

  data Input =    Literal AtomicType
                  | InAddr Address
  data Output =   OutAddr Address

  data TAC =      AddCode Input Input Output
                  | SubCode Input Input Output
                  | MulCode Input Input Output
                  | DivCode Input Input Output