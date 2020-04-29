module Type where

  import Environment

  data AtomicType =       VoidType
                          | CharType
                          | IntType
                          deriving (Show, Eq, Ord)

  data Type =             Atom AtomicType
                          | ArrowType [Type] Type
                          | ArrayType Int Type
                          | PointerType Type
                          deriving (Show, Eq)

  type TypeEnvironment =  GenericEnvironment Type

  lowestAtomicType :: AtomicType
  lowestAtomicType = CharType

  castAtomicTypes :: AtomicType -> AtomicType -> AtomicType
  castAtomicTypes a b
    | a == b = a
    | otherwise = max a b
