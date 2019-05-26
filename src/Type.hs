module Type where

    data AtomicType =   IntType
                        | CharType
                        | VoidType
                        deriving (Show, Eq)

    data Type =         Atom AtomicType
                        | ArrowType [Type] Type
                        | ArrayType Int Type
                        deriving (Show, Eq)