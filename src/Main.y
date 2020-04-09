{
module Main where

import Control.Monad.State
import Data.Char
import Data.List
import Debug.Trace
import System.Environment

import AST
import Environment
import ThreeAddressCode
import Type
import TypeChecking
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
      TNumber                   { TNumber number }
      TQChar                    { TQChar char    }
      TVoid                     { TVoid          }
      TEnd                      { TEnd           }

%left TAssign
%left TEqual TNequal
%left TGreater TGreaterEqual TLess TLessEqual
%left TPlus TMinus
%left TTimes TDivide
%right TLength TNot

%%

program:                        declarations                                              { $1 :: (State AST.Location Declarations) }
                                
declarations:                   declaration declarations                                  { do decl <- $1; decls <- $2; return $ decl : decls }
                                |                                                         { return [] }

declaration:                    var_declaration                                           { $1 }
                                | fun_definition                                          { $1 }

type:                           TInt                                                      { return $ Atom IntType }
                                | TChar                                                   { return $ Atom CharType }
                                | TVoid                                                   { return $ Atom VoidType }

var_declaration:                type TName TSemicolon                                     { do typ <- $1; return $ VarDeclaration typ $2 }

var_declarations:               var_declaration var_declarations                          { do decl <- $1; decls <- $2; return $ decl : decls }
                                |                                                         { return [] }

fun_definition:                 type TName TLpar formal_pars TRpar block                  { do typ <- $1; decls <- $4; body <- $6; return $ FunDeclaration typ $2 decls body }

formal_pars:                    formal_pars_tail                                          { $1 }
                                |                                                         { return [] }
                                
                                
formal_pars_tail:               formal_par                                                { $1 >>= \(par) ->  return [par] }
                                | formal_par TComma formal_pars_tail                      { do par <- $1; pars <- $3; return $ par : pars  }

formal_par:                     type TName                                                { do typ <- $1; return $ VarDeclaration typ $2 }

block:                          TLbrace var_declarations statements TRbrace               { do decls <- $2; stmts <- $3; return $ Body decls stmts }

statement:                      lexp TAssign exp                                          { do lexp <- $1; exp <- $3; return $ AssignStmt lexp exp }
                                | TReturn exp                                             { $2 >>= \(exp) -> return $ ReturnStmt exp }
                                | TRead lexp                                              { $2 >>= \(lexp) -> return $ ReadStmt lexp }
                                | TWrite lexp                                             { $2 >>= \(lexp) -> return $ WriteStmt lexp }
                                | TIf TLpar exp TRpar block TElse marker block marker     { do pred <- $3; blk1 <- $5; mrk1 <- $7; blk2 <- $8; mrk2 <- $9; return $ IfStmt pred blk1 mrk1 blk2 mrk2 }
                                | block                                                   { $1 >>= \(blk) -> return $ BlockStmt blk }
                                | exp                                                     { $1 >>= \(exp) -> return $ ExpStmt exp }

statements:                     statement statement_semicolon                             { do stmt <- $1; stmts <- $2; return $ stmt : stmts }
                                |                                                         { return [] }

statement_semicolon:            TSemicolon statements                                     { $2 >>= \(stmt) -> return stmt }
                                |                                                         { return [] }

lexp:                           TName                                                     { return $ VariableRefExp $1 }
                                | exp TLbrack exp TRbrack                                 { do exp1 <- $1; exp2 <- $3; return $ ArrayRefExp exp1 exp2 }

exp:                            lexp                                                      { $1 >>= \(lexp) -> return $ LeftExp lexp }
                                | binopExp                                                { $1 >>= \(exp) -> return exp }
                                | unopExp                                                 { $1 >>= \(exp) -> return exp }
                                | TLpar exp TRpar                                         { $2 >>= \(exp) -> return exp }
                                | funCallExp                                              { $1 >>= \(exp) -> return exp }
                                | TLength exp                                             { $2 >>= \(exp) -> return $ LengthExp exp }
                                | TNumber                                                 { return $ NumberExp $ number $1 }
                                | TQChar                                                  { return $ QCharExp $ char $1 }

funCallExp:                     TName TLpar pars TRpar                                    { do pars <- $3; return $ FunctionAppExp $1 pars }

binopExp:                       exp TPlus exp                                             { do exp1 <- $1; exp2 <- $3; return $ BinaryExp PlusOp exp1 exp2 }
                                | exp TMinus exp                                          { do exp1 <- $1; exp2 <- $3; return $ BinaryExp MinusOp exp1 exp2 }
                                | exp TTimes exp                                          { do exp1 <- $1; exp2 <- $3; return $ BinaryExp TimesOp exp1 exp2 }
                                | exp TDivide exp                                         { do exp1 <- $1; exp2 <- $3; return $ BinaryExp DivideOp exp1 exp2 }
                                | exp TEqual exp                                          { do exp1 <- $1; exp2 <- $3; return $ BinaryExp EqualOp exp1 exp2 }
                                | exp TNequal exp                                         { do exp1 <- $1; exp2 <- $3; return $ BinaryExp NequalOp exp1 exp2 }
                                | exp TGreater exp                                        { do exp1 <- $1; exp2 <- $3; return $ BinaryExp GreaterOp exp1 exp2 }
                                | exp TGreaterEqual exp                                   { do exp1 <- $1; exp2 <- $3; return $ BinaryExp GreaterEqualOp exp1 exp2 }
                                | exp TLess exp                                           { do exp1 <- $1; exp2 <- $3; return $ BinaryExp LessOp exp1 exp2 }
                                | exp TLessEqual exp                                      { do exp1 <- $1; exp2 <- $3; return $ BinaryExp LessEqualOp exp1 exp2 }

unopExp:                        TNot exp                                                  { $2 >>= \(exp) -> return $ UnaryExp NotOp exp }

pars:                           parTail                                                   { $1 >>= \(pars) -> return pars }
                                | {- empty -}                                             { return [] }

parTail:                        exp                                                       { $1 >>= \(exp) -> return [exp] }
                                | exp TComma parTail                                      { do exp <- $1; pars <- $3; return $ exp : pars }

marker:                         {- empty -}                                               { do loc <- get; nextLoc; return $ Marker loc }

{

nilLocation :: AST.Location
nilLocation = 0


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
     | TVoid
     | TLess
     | TLessEqual
     | TComment
     | TNumber { number :: Integer }
     | TQChar { char :: Char }
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
lexer ('!' : 'x' :cs)   =     TNot : lexer cs
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
lexer ('\'':c:'\'':cs)  =     TQChar c : lexer cs

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
      ("void", rest )   ->    TVoid : lexer rest
      ("while",rest)    ->    TWhile : lexer rest
      ("write",rest)    ->    TWrite : lexer rest
      (var,rest)        ->    TName var : lexer rest

main = do args <- getArgs
          if (length args) /= 1
             then putStrLn "Exactly one argument, filename, expected"
             else (readFile $ head args) >>= \(fileContent) ->
                  let (decls, _) = runState (parser $ lexer fileContent) nilLocation
                  in do print "##### TYPES #####"
                        print $ TypeChecking.typeCheckDeclarations Environment.empty decls
                        print "##### DECLARATIONS #####"
                        putStrLn $ concat (intersperse "\n" $ map show decls)
                        print "##### TACS #####"
                        putStrLn $ concat (intersperse "\n" . map show $ generateTACs decls)
}