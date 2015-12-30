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
      '\t'                  { TTab       }
      '\n'                  { TNewline   }
      int                   { TInt       }
      return                { TReturn    }
      '}'                   { TRbrace    }
      TSemicolon            { TSemicolon }
      '*'                   { TTimes     }
      write                 { TWrite     }
      '!'                   { TNot       }
      if                    { TIf        }
      '('                   { TLpar      }
      '['                   { TLbrack    }
      ','                   { TComma     }
      '/'                   { TDivide    }
      read                  { TRead      }
      length                { TLength    }
      else                  { TElse      }
      ')'                   { TRpar      }
      ']'                   { TRbrack    }
      '+'                   { TPlus      }
      '>'                   { TGreater   }
      while                 { TWhile     }
      "=="                  { TEqual     }
      "!="                  { TNequal    }
      '{'                   { TLbrace    }
      '='                   { TAssign    }
      '-'                   { TMinus     }
      char                  { TChar      }
      '<'                   { TLess      }
      name                  { TName $$   }
      TEnd                  { TEnd       }

%%

program:                        declarations                              { $1 }
                                
declarations:                   var_declaration                           { [$1] }

var_declaration:                type name TSemicolon                      { VarDeclaration $1 $2 }

type:                           int                                       { IntType }

{

data TypeName = IntType
                | CharType
                deriving (Show, Eq)

data Declaration = VarDeclaration TypeName String
                   deriving (Show, Eq)

type Declarations = [Declaration]


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
     | TWhile
     | TEqual
     | TNequal
     | TLbrace
     | TAssign
     | TMinus
     | TInt
     | TChar
     | TLess
     | TComment
     | TNumber Int
     | TQchar Char
     | TName String
     deriving Show

lexer :: String -> [Token]
lexer []                = []
lexer (c:cs)
      | isSpace c       = lexer cs
      | isAlpha c       = lexName (c:cs)
      | isDigit c       = lexNum (c:cs)
lexer ('=':'=':cs)      = TEqual : lexer cs
lexer ('=':'!':'=':cs)  = TNequal : lexer cs
lexer ('=':cs)          = TAssign : lexer cs
lexer ('+':cs)          = TPlus : lexer cs
lexer ('-':cs)          = TMinus : lexer cs
lexer ('*':cs)          = TTimes : lexer cs
lexer ('/':cs)          = TDivide : lexer cs
lexer (';':cs)          = TSemicolon : lexer cs
lexer ('!':cs)          = TNot : lexer cs
lexer ('(':cs)          = TLpar : lexer cs
lexer (')':cs)          = TRpar : lexer cs
lexer ('{':cs)          = TLbrace : lexer cs
lexer ('}':cs)          = TRbrace : lexer cs
lexer ('[':cs)          = TLbrack : lexer cs
lexer (']':cs)          = TRbrack : lexer cs
lexer ('>':cs)          = TGreater : lexer cs
lexer ('<':cs)          = TLess : lexer cs
lexer ('\n':'\n':cs)    = TEnd : lexer cs
lexer ('\t':cs)         = TTab : lexer cs
lexer (',':cs)          = TComma : lexer cs
lexer ('\'':c:'\'':cs)  = TQchar c : lexer cs

lexNum cs = TNumber (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexName cs =
   case span isAlpha cs of
      ("char",rest) ->    TChar : lexer rest
      ("else",rest) ->    TElse : lexer rest
      ("if",rest)  ->     TIf : lexer rest
      ("int",rest)  ->    TInt : lexer rest
      ("length",rest) ->  TLength : lexer rest
      ("read",rest) ->    TRead : lexer rest
      ("return",rest) ->  TReturn : lexer rest
      ("while",rest) ->   TWhile : lexer rest
      ("write",rest) ->   TWrite : lexer rest
      (var,rest)   ->     TName var : lexer rest

main = do args <- getArgs
          if (length args) /= 1
             then putStrLn "Exactly one argument, filename, expected"
             else (readFile $ head args) >>= print . parser . lexer
}