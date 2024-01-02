{
module Main where

import Control.Monad.State
import Data.Char
import Data.List
import Debug.Trace
import System.Environment
import System.Exit

import AST
import Environment
-- import MASM_86_Compile
-- import TASM_86_Compile
import NASM_86_Compile
import Remove_Redundant_TACS
import ThreeAddressCode
import Traverse_AST_Array_Ref
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
      TAmpersand                { TAmpersand     }
      TNot                      { TNot           }
      TInc                      { TInc           }
      TDec                      { TDec           }
      TIf                       { TIf            }
      TLpar                     { TLpar          }
      TLbrack                   { TLbrack        }
      TComma                    { TComma         }
      TDivide                   { TDivide        }
      TRead                     { TRead          }
      TLength                   { TLength        }
      TElse                     { TElse          }
      TFor                      { TFor           }
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
      TAssignPlus               { TAssignPlus    }
      TAssignMinus              { TAssignMinus   }
      TAssignTimes              { TAssignTimes   }
      TAssignDiv                { TAssignDiv     }
      TAssignModulo             { TAssignModulo  }
      TMinus                    { TMinus         }
      TChar                     { TChar          }
      TLess                     { TLess          }
      TLessEqual                { TLessEqual     }
      TName                     { TName $$       }
      TNumber                   { TNumber number }
      TQChar                    { TQChar char    }
      TVoid                     { TVoid          }
      TEnd                      { TEnd           }

%left TAssign TAssignPlus TAssignMinus TAssignTimes TAssignDiv TAssignModulo
%left TEqual TNequal
%left TGreater TGreaterEqual TLess TLessEqual
%left TPlus TMinus
%left TTimes TDivide
%right TLength TNot TAmpersand

%%

program:                        declarations                                              { $1 :: (State AST.Location [Declaration ()]) }
                                
declarations:                   declaration declarations                                  { do decl <- $1; decls <- $2; return (decl : decls) :: State AST.Location [Declaration ()] }
                                |                                                         { (return []) :: (State AST.Location [Declaration ()]) }

declaration:                    var_declaration                                           { $1 :: State AST.Location (Declaration ()) }
                                | fun_definition                                          { $1 :: State AST.Location (Declaration ()) }

type:                           TInt                                                      { return $ Atom IntType }
                                | TChar                                                   { return $ Atom CharType }
                                | TVoid                                                   { return $ Atom VoidType }
                                | type TTimes                                             { do typ <- $1; return $ PointerType typ }

var_declaration:                type TName TSemicolon                                     { do typ <- $1; return $ VarDeclaration typ $2 () }
                                | type TName TLbrack TNumber TRbrack TSemicolon           { do typ <- $1; return $ VarDeclaration (ArrayType (number $4) typ) $2 () }
                                | type TName TEqual exp                                   { do typ <- $1; exp <- $4; return $ VarDefinition typ $2 exp () }

var_declarations:               var_declaration var_declarations                          { do decl <- $1; decls <- $2; return (decl : decls) }
                                |                                                         { return [] }

fun_definition:                 type TName TLpar formal_pars TRpar block                  { do typ <- $1; decls <- $4; body <- $6; return $ FunDeclaration typ $2 decls body () }

formal_pars:                    formal_pars_tail                                          { $1 :: State AST.Location (Declarations ()) }
                                |                                                         { (return []) :: State AST.Location (Declarations ()) }
                                
                                
formal_pars_tail:               formal_par                                                { $1 >>= \(par) ->  return [par] }
                                | formal_par TComma formal_pars_tail                      { do par <- $1; pars <- $3; return (par : pars)  }

formal_par:                     type TName                                                { do typ <- $1; return $ VarDeclaration typ $2 () }
                                | type TName TLbrack TNumber TRbrack                      { do typ <- $1; return $ VarDeclaration (PointerType typ) $2 () }
                                | type TName TLbrack TRbrack                              { do typ <- $1; return $ VarDeclaration (PointerType typ) $2 () }

block:                          TLbrace var_declarations statements TRbrace               { do decls <- $2; stmts <- $3; return $ Body decls stmts () }

statement:                      lexp TAssign exp                                          { do lexp <- $1; exp <- $3; return $ AssignStmt lexp exp () }
                                | lexp TAssignPlus exp                                    { do lexp <- $1; exp <- $3; return $ AssignStmt lexp (BinaryExp PlusOp (LeftExp lexp ()) exp ()) () }
                                | lexp TAssignMinus exp                                   { do lexp <- $1; exp <- $3; return $ AssignStmt lexp (BinaryExp MinusOp (LeftExp lexp ()) exp ()) () }
                                | lexp TAssignTimes exp                                   { do lexp <- $1; exp <- $3; return $ AssignStmt lexp (BinaryExp TimesOp (LeftExp lexp ()) exp ()) () }
                                | lexp TAssignDiv exp                                     { do lexp <- $1; exp <- $3; return $ AssignStmt lexp (BinaryExp DivideOp (LeftExp lexp ()) exp ()) () }
                                | lexp TAssignModulo exp                                  { do lexp <- $1; exp <- $3; return $ AssignStmt lexp (BinaryExp ModuloOp (LeftExp lexp ()) exp ()) () }
                                | TReturn                                                 { return $ Return0Stmt () }
                                | TReturn exp                                             { $2 >>= \(exp) -> return $ Return1Stmt exp () }
                                | TRead lexp                                              { $2 >>= \(lexp) -> return $ ReadStmt lexp () }
                                | TWrite lexp                                             { $2 >>= \(lexp) -> return $ WriteStmt lexp () }
                                | TIf TLpar exp TRpar block TElse marker block marker     { do pred <- $3; blk1 <- $5; mrk1 <- $7; blk2 <- $8; mrk2 <- $9; return $ IfElseStmt pred blk1 mrk1 blk2 mrk2 () }
                                | TIf TLpar exp TRpar block marker                        { do pred <- $3; blk1 <- $5; mrk1 <- $6; return $ IfStmt pred blk1 mrk1 () }
                                | block                                                   { $1 >>= \(blk) -> return $ BlockStmt blk () }
                                | exp                                                     { $1 >>= \(exp) -> return $ ExpStmt exp () }
                                | TWhile marker TLpar exp TRpar block marker              { do mrk1 <- $2; pred <- $4; block <- $6; mrk2 <- $7; return $ WhileStmt mrk1 pred block mrk2 () }
                                | TFor TLpar statement TSemicolon marker exp TSemicolon statement marker TRpar marker block marker                       { do mrk1 <- $5; mrk2 <- $9; mrk3 <- $11; mrk4 <- $13; initStmt <- $3; pred <- $6; incStmt <- $8; body <- $12; return $ ForStmt initStmt mrk1 pred mrk2 incStmt mrk3 body mrk4 () }

statements:                     statement statement_semicolon                             { do stmt <- $1; stmts <- $2; return (stmt : stmts) }
                                |                                                         { return [] }

statement_semicolon:            TSemicolon statements                                     { $2 >>= \(stmt) -> return stmt }
                                |                                                         { return [] }

pexp:                           TName                                                     { return $ PointerExp (VariableRefExp $1 ()) (NumberExp 0 ()) () }

lexp:                           TName                                                     { return $ VariableRefExp $1 () }
                                | lexp TLbrack exp TRbrack                                { do exp1 <- $1; exp2 <- $3; return $ ArrayRefExp exp1 exp2 () }
                                | TTimes pexp                                             { do pexp <- $2; return $ DerefExp pexp () }

exp:                            lexp                                                      { $1 >>= \(lexp) -> return $ LeftExp lexp () }
                                | binopExp                                                { $1 >>= \(exp) -> return exp }
                                | unopExp                                                 { $1 >>= \(exp) -> return exp }
                                | TLpar exp TRpar                                         { $2 >>= \(exp) -> return exp }
                                | funCallExp                                              { $1 >>= \(exp) -> return exp }
                                | TLength exp                                             { $2 >>= \(exp) -> return $ LengthExp exp () }
                                | TNumber                                                 { return $ NumberExp (number $1) () }
                                | TQChar                                                  { return $ QCharExp (char $1) () }

funCallExp:                     TName TLpar pars TRpar                                    { do pars <- $3; return $ FunctionAppExp $1 pars () }

binopExp:                       exp TPlus exp                                             { do exp1 <- $1; exp2 <- $3; return $ BinaryExp PlusOp exp1 exp2 () }
                                | exp TMinus exp                                          { do exp1 <- $1; exp2 <- $3; return $ BinaryExp MinusOp exp1 exp2 () }
                                | exp TTimes exp                                          { do exp1 <- $1; exp2 <- $3; return $ BinaryExp TimesOp exp1 exp2 () }
                                | exp TDivide exp                                         { do exp1 <- $1; exp2 <- $3; return $ BinaryExp DivideOp exp1 exp2 () }
                                | exp TEqual exp                                          { do exp1 <- $1; exp2 <- $3; return $ BinaryExp EqualOp exp1 exp2 () }
                                | exp TNequal exp                                         { do exp1 <- $1; exp2 <- $3; return $ BinaryExp NequalOp exp1 exp2 () }
                                | exp TGreater exp                                        { do exp1 <- $1; exp2 <- $3; return $ BinaryExp GreaterOp exp1 exp2 () }
                                | exp TGreaterEqual exp                                   { do exp1 <- $1; exp2 <- $3; return $ BinaryExp GreaterEqualOp exp1 exp2 () }
                                | exp TLess exp                                           { do exp1 <- $1; exp2 <- $3; return $ BinaryExp LessOp exp1 exp2 () }
                                | exp TLessEqual exp                                      { do exp1 <- $1; exp2 <- $3; return $ BinaryExp LessEqualOp exp1 exp2 () }

unopExp:                        TNot exp                                                  { $2 >>= \(exp) -> return $ UnaryExp NotOp exp () }
                                | TInc lexp                                               { $2 >>= \(exp) -> return $ UnaryModifyingExp PrefixIncOp exp () }
                                | lexp TInc                                               { $1 >>= \(exp) -> return $ UnaryModifyingExp SuffixIncOp exp () }
                                | TDec lexp                                               { $2 >>= \(exp) -> return $ UnaryModifyingExp PrefixDecOp exp () }
                                | lexp TDec                                               { $1 >>= \(exp) -> return $ UnaryModifyingExp SuffixDecOp exp () }
                                | TAmpersand lexp                                         { do lexp <- $2; return $ AddressOf lexp () }

pars:                           parTail                                                   { $1 >>= \(pars) -> return pars }
                                | {- empty -}                                             { return [] }

parTail:                        exp                                                       { $1 >>= \(exp) -> return [exp] }
                                | exp TComma parTail                                      { do exp <- $1; pars <- $3; return (exp : pars) }

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
     | TAmpersand
     | TWrite
     | TFor
     | TNot
     | TInc
     | TDec
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
     | TAssignPlus
     | TAssignMinus
     | TAssignTimes
     | TAssignDiv
     | TAssignModulo
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
lexer ('+':'=':cs)      =     TAssignPlus : lexer cs
lexer ('-':'=':cs)      =     TAssignMinus : lexer cs
lexer ('*':'=':cs)      =     TAssignTimes : lexer cs
lexer ('/':'=':cs)      =     TAssignDiv : lexer cs
lexer ('%':'=':cs)      =     TAssignModulo : lexer cs
lexer ('+':'+':cs)      =     TInc : lexer cs
lexer ('-':'-':cs)      =     TDec : lexer cs
lexer ('=':cs)          =     TAssign : lexer cs
lexer ('+':cs)          =     TPlus : lexer cs
lexer ('-':cs)          =     TMinus : lexer cs
lexer ('*':cs)          =     TTimes : lexer cs
lexer ('/':cs)          =     TDivide : lexer cs
lexer ('&':cs)          =     TAmpersand : lexer cs
lexer (';':cs)          =     TSemicolon : lexer cs
lexer ('!':'=':cs)      =     TNequal : lexer cs
lexer ('!':cs)          =     TNot : lexer cs
lexer ('(':cs)          =     TLpar : lexer cs
lexer (')':cs)          =     TRpar : lexer cs
lexer ('{':cs)          =     TLbrace : lexer cs
lexer ('}':cs)          =     TRbrace : lexer cs
lexer ('[':cs)          =     TLbrack : lexer cs
lexer (']':cs)          =     TRbrack : lexer cs
lexer ('>':'=':cs)      =     TGreaterEqual : lexer cs
lexer ('>':cs)          =     TGreater : lexer cs
lexer ('<':'=':cs)      =     TLessEqual : lexer cs
lexer ('<':cs)          =     TLess : lexer cs
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
      ("for",rest)      ->    TFor : lexer rest
      (var,rest)        ->    TName var : lexer rest

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b

data Config = Config { fileName' :: String, verbose' :: Bool, outputFileName' :: String } deriving (Show, Eq)
defaultConfig :: Config
defaultConfig = Config "" False ""

handleArg :: Config -> (String, Integer) -> Config
handleArg config ("--verbose", _) = config {verbose' = True}
handleArg config ("--silent", _) = config {verbose' = False}
handleArg config (fileName, 0) = config {fileName' = fileName}
handleArg config (fileName, 1) = config {outputFileName' = fileName}

main = do args <- getArgs
          let config = foldl (\config arg -> handleArg config arg) defaultConfig $ zip args [0..]
          let verbose = verbose' config
          let fileName = fileName' config
          let outputFileName = outputFileName' config
          if (fileName == "") || (outputFileName == "")
             then putStrLn "Two arguments, input file name and output file name, expected"
             else (readFile fileName) >>= \(fileContent) ->
                  let (decls, _) = runState (parser $ lexer fileContent) nilLocation
                      decls' = transformDeclarations decls
                  in do when verbose $ print "##### Declarations #####"
                        when verbose . putStrLn $ concat (intersperse "\n" $ map show decls')
                        -- return $ walkDeclarations2 (map (\decl -> changeDeclarationT decl (\_ -> ASTTransformerChanger)) decls)
                        when verbose $ print "##### Types #####"
                        let typeChecked = TypeChecking.typeCheck decls'
                        when verbose . print $ typeChecked
                        -- either (\_ -> exitWith $ ExitFailure 1) (\types -> when verbose . print $ types) typeChecked
                        let tDecls = typeChecked
                        when verbose $ print "##### TACs #####"
                        let tacFile = generateTACs tDecls
                        when verbose $ print tacFile
                        -- putStrLn $ concat (intersperse "\n" . map show $ generateTACs tDecls)
                        when verbose $ print "##### Optimised TACs #####"
                        let optimisedTacFile = optimiseTACFile tacFile
                        when verbose $ print optimisedTacFile
                        when verbose $ print "##### Assembly #####"
                        let compiled = compile optimisedTacFile
                        when verbose $ putStrLn compiled
                        writeFile outputFileName compiled
                        exitWith ExitSuccess
  }