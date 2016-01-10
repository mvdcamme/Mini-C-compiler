{
module Main where

import Data.Char
import Debug.Trace
import System.Environment
}

%name parser
%tokentype { Token }
%error { parseError }

%token
      TTab                      { TTab           }
      TNewline                  { TNewline       }
      TInt                      { TInt           }
      TReturn                   { TReturn        }
      TRbrace                   { TRbrace        }
      TSemicolon                { TSemicolon     }
      TTimes                    { TTimes         }
      TWrite                    { TWrite         }
      TNot                      { TNot           }
      TIf                       { TIf            }
      TLpar                     { TLpar          }
      TLbrack                   { TLbrack        }
      TComma                    { TComma         }
      TDivide                   { TDivide        }
      TRead                     { TRead          }
      TLength                   { TLength        }
      TElse                     { TElse          }
      TRpar                     { TRpar          }
      TRbrack                   { TRbrack        }
      TPlus                     { TPlus          }
      TGreater                  { TGreater       }
      TGreaterEqual             { TGreaterEqual  }
      TWhile                    { TWhile         }
      TEqual                    { TEqual         }
      TNequal                   { TNequal        }
      TLbrace                   { TLbrace        }
      TAssign                   { TAssign        }
      TMinus                    { TMinus         }
      TChar                     { TChar          }
      TLess                     { TLess          }
      TLessEqual                { TLessEqual     }
      TName                     { TName $$       }
      TEnd                      { TEnd           }

%%

program:                        declarations                                        { $1 }
                                
declarations:                   declaration declarations                            { $1 : $2 }
                                |                                                   { [] }

declaration:                    var_declaration                                     { $1 }
                                | fun_definition                                    { $1 }

type:                           TInt                                                { Atom IntType }
                                | TChar                                             { Atom CharType}

var_declaration:                type TName TSemicolon                               { VarDeclaration $1 $2 }

var_declarations:               var_declaration var_declarations                    { $1 : $2 }
                                |                                                   { [] }

fun_definition:                type TName TLpar formal_pars TRpar block             { FunDeclaration $1 $2 $4 $6 }

formal_pars:                    formal_pars_tail                                    { $1 }
                                |                                                   { [] }
                                
                                
formal_pars_tail:               formal_par                                          { [$1] }
                                | formal_par TComma formal_pars_tail                { $1 : $3 }

formal_par:                     type TName                                          { VarDeclaration $1 $2 }

block:                          TLbrace var_declarations statements TRbrace         { Body $2 $3 }

statement:                      TName TAssign                                       { SAssign $1 }

statements:                     statement statement_semicolon                       { $1 : $2 }
                                |                                                   { [] }

statement_semicolon:            TSemicolon statements                               { $2 }
                                |                                                   { [] }

lexp:                           TName                                               { VariableRefExp $1 }
                                | lexp TLbrack exp TRbrack                          { ArrayRefExp $1 $3 }

exp:                            lexp                                                { $1 }
                                | binopExp                                          { $1 }
                                | unopExp                                           { $1}
                                | TLpar exp TRpar                                   { $2 }
                                | TName TLpar pars TRpar                            { FunctionAppExp $1 $3 }
                                | TLength lexp                                      { LengthExp $2 }

binopExp:                       exp binOperator exp                                 { BinaryExp $2 $1 $3 }

binOperator:                    TPlus                                               { PlusOp }
                                | TMinus                                            { MinusOp }
                                | TTimes                                            { TimesOp }
                                | TDivide                                           { DivideOp }
                                | TEqual                                            { EqualOp }
                                | TNequal                                           { NequalOp }
                                | TGreater                                          { GreaterOp }
                                | TGreaterEqual                                     { GreaterEqualOp }
                                | TLess                                             { LessOp }
                                | TLessEqual                                        { LessEqualOp }

unopExp:                        unOperator exp                                      { UnaryExp $1 $2}

unOperator:                     TMinus                                              { MinusUnOp }
                                | TNot                                              { NotOp }

pars:                           exp pars                                            { $1 : $2 }
                                |                                                   { [] }

{

type Name =                     String

data AtomicType =               IntType
                                | CharType
                                deriving (Show, Eq)

data Type =                     Atom AtomicType
                                | ArrayType Int Type
                                deriving (Show, Eq)

data Declaration =              VarDeclaration Type Name
                                | FunDeclaration Type Name Declarations Body
                                deriving (Show, Eq)

type Declarations =             [Declaration]

data Statement =                SAssign Name
                                deriving (Show, Eq)

type Statements =               [Statement]

data Body =                     Body Declarations Statements
                                deriving (Show, Eq)

data Expression =               VariableRefExp Name
                                | ArrayRefExp Expression Expression
                                | BinaryExp BinOperator Expression Expression
                                | UnaryExp UnOperator Expression
                                | FunctionAppExp Name Expressions
                                | LengthExp Expression
                                | NumberExp 
                                deriving (Show, Eq)

type Expressions =              [Expression]

data BinOperator =              PlusOp
                                | MinusOp
                                | TimesOp
                                | DivideOp
                                | EqualOp
                                | NequalOp
                                | GreaterOp
                                | GreaterEqualOp
                                | LessOp
                                | LessEqualOp
                                deriving (Show, Eq)

data UnOperator =               MinusUnOp
                                | NotOp
                                deriving (Show, Eq)


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
     = TTab
     | TNewline
     | TEnd
     | TReturn
     | TRbrace
     | TSemicolon
     | TTimes
     | TWrite
     | TNot
     | TIf
     | TLpar
     | TLbrack
     | TComma
     | TDivide
     | TRead
     | TLength
     | TElse
     | TRpar
     | TRbrack
     | TPlus
     | TGreater
     | TGreaterEqual
     | TWhile
     | TEqual
     | TNequal
     | TLbrace
     | TAssign
     | TMinus
     | TInt
     | TChar
     | TLess
     | TLessEqual
     | TComment
     | TNumber Int
     | TQchar Char
     | TName String
     deriving Show

lexer :: String -> [Token]
lexer []                =     []
lexer (c:cs)
      | isSpace c       =     lexer cs
      | isAlpha c       =     lexName (c:cs)
      | isDigit c       =     lexNum (c:cs)
lexer ('=':'=':cs)      =     TEqual : lexer cs
lexer ('=':'!':'=':cs)  =     TNequal : lexer cs
lexer ('=':cs)          =     TAssign : lexer cs
lexer ('+':cs)          =     TPlus : lexer cs
lexer ('-':cs)          =     TMinus : lexer cs
lexer ('*':cs)          =     TTimes : lexer cs
lexer ('/':cs)          =     TDivide : lexer cs
lexer (';':cs)          =     TSemicolon : lexer cs
lexer ('!':cs)          =     TNot : lexer cs
lexer ('(':cs)          =     TLpar : lexer cs
lexer (')':cs)          =     TRpar : lexer cs
lexer ('{':cs)          =     TLbrace : lexer cs
lexer ('}':cs)          =     TRbrace : lexer cs
lexer ('[':cs)          =     TLbrack : lexer cs
lexer (']':cs)          =     TRbrack : lexer cs
lexer ('>':cs)          =     TGreater : lexer cs
lexer ('>':'=':cs)      =     TGreaterEqual : lexer cs
lexer ('<':cs)          =     TLess : lexer cs
lexer ('<':'=':cs)      =     TLessEqual : lexer cs
lexer ('\n':'\n':cs)    =     TEnd : lexer cs
lexer ('\t':cs)         =     TTab : lexer cs
lexer (',':cs)          =     TComma : lexer cs
lexer ('\'':c:'\'':cs)  =     TQchar c : lexer cs

lexNum cs = TNumber (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexName cs =
   case span isAlpha cs of
      ("char",rest)     ->    TChar : lexer rest
      ("else",rest)     ->    TElse : lexer rest
      ("if",rest)       ->    TIf : lexer rest
      ("int",rest)      ->    TInt : lexer rest
      ("length",rest)   ->    TLength : lexer rest
      ("read",rest)     ->    TRead : lexer rest
      ("return",rest)   ->    TReturn : lexer rest
      ("while",rest)    ->    TWhile : lexer rest
      ("write",rest)    ->    TWrite : lexer rest
      (var,rest)        ->    TName var : lexer rest

main = do args <- getArgs
          if (length args) /= 1
             then putStrLn "Exactly one argument, filename, expected"
             else (readFile $ head args) >>= print . parser . lexer
}