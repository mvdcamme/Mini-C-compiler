module Traverse_AST_Array_Ref where

  import AST

  data ASTTransformer t = ASTTransformer { transformBody :: Body t -> Body t
                                         , transformDeclaration :: Declaration t -> Declaration t
                                         , transformExp :: Expression t -> Expression t
                                         , transformLExp :: LeftExpression t -> LeftExpression t
                                         , transformPExp :: PointerExpression t -> PointerExpression t
                                         , transformStatement :: Statement t -> Statement t
                                         }

  -- Attempt 1

  transformArrayExpToDerefExp :: LeftExpression t -> LeftExpression t
  transformArrayExpToDerefExp (ArrayRefExp lexp exp t) = DerefExp (PointerExp lexp exp t) t
  transformArrayExpToDerefExp other = other

  idTransformer :: ASTTransformer t
  idTransformer = ASTTransformer id id id id id id
  arrayRefTransformer :: ASTTransformer t
  arrayRefTransformer = ASTTransformer id id id transformArrayExpToDerefExp id id

  walkDeclaration :: ASTTransformer t -> Declaration t -> Declaration t
  walkDeclaration transformer decl@(VarDeclaration typ name t) =
    transformDeclaration transformer decl
  walkDeclaration transformer decl@(FunDeclaration typ name decls body t) =
    transformDeclaration transformer $
      FunDeclaration typ name (walkDeclarations transformer decls) (walkBody transformer body) t

  walkExp :: ASTTransformer t -> Expression t -> Expression t
  walkExp transformer (LeftExp lexp t) =
    transformExp transformer $
      LeftExp (walkLExp transformer lexp) t
  walkExp transformer (BinaryExp binop left right t) =
    transformExp transformer $
      BinaryExp binop
                (walkExp transformer left)
                (walkExp transformer right)
                t
  walkExp transformer (UnaryExp unop exp t) =
    transformExp transformer $
      UnaryExp unop
               (walkExp transformer exp)
               t
  walkExp transformer (UnaryModifyingExp unop lexp t) =
    transformExp transformer $
      UnaryModifyingExp unop
                        (walkLExp transformer lexp)
                        t
  walkExp transformer (FunctionAppExp name exps t) =
    transformExp transformer $
      FunctionAppExp name
                     (walkExps transformer exps)
                     t
  walkExp transformer (FunctionAppWithTypedExp name exps t) =
    transformExp transformer $
      FunctionAppWithTypedExp name
                              (map (\(exp, typ) -> (walkExp transformer exp, typ)) exps)
                              t
  walkExp transformer (LengthExp exp t) =
    transformExp transformer $
      LengthExp (walkExp transformer exp)
                t
  walkExp transformer (AddressOf lexp t) =
    transformExp transformer $
      AddressOf (walkLExp transformer lexp)
                t
  walkExp transformer exp@(NumberExp lexp t) = exp
  walkExp transformer exp@(QCharExp lexp t) = exp

  walkLExp :: ASTTransformer t -> LeftExpression t -> LeftExpression t
  walkLExp transformer lexp@(VariableRefExp name t) =
    transformLExp transformer lexp
  walkLExp transformer (ArrayRefExp lexp exp t) =
    transformLExp transformer $
      ArrayRefExp (walkLExp transformer lexp) (walkExp transformer exp) t
  walkLExp transformer (DerefExp pexp t) =
    transformLExp transformer $
      DerefExp (walkPExp transformer pexp) t

  walkPExp :: ASTTransformer t -> PointerExpression t -> PointerExpression t
  walkPExp transformer (PointerExp lexp exp t) =
    transformPExp transformer $ PointerExp (walkLExp transformer lexp) (walkExp transformer exp) t

  walkStatement :: ASTTransformer t -> Statement t -> Statement t
  walkStatement transformer (AssignStmt lexp exp t) =
    transformStatement transformer $
      AssignStmt (walkLExp transformer lexp) (walkExp transformer exp) t
  -- walkStatement transformer (AssignOpStmt op lexp exp t) =
  --     let newRExp = BinaryExp (assignableBinOpToBinOp op) (LeftExp lexp t) exp t
  --     in transformStatement transformer $
  --          AssignStmt (walkLExp transformer lexp) (walkExp transformer newRExp) t
  walkStatement transformer (BlockStmt body t) =
    transformStatement transformer $
      BlockStmt (walkBody transformer body) t
  walkStatement transformer (ExpStmt exp t) =
    transformStatement transformer $
      ExpStmt (walkExp transformer exp) t
  walkStatement transformer (ForStmt init m1 pred m2 inc m3 body m4 t) =
    transformStatement transformer $
      ForStmt (walkStatement transformer init)
              m1
              (walkExp transformer pred)
              m2
              (walkStatement transformer inc)
              m3
              (walkBody transformer body)
              m4
              t
  walkStatement transformer (IfElseStmt pred thenBody m1 elseBody m2 t) =
    transformStatement transformer $
      IfElseStmt (walkExp transformer pred)
                 (walkBody transformer thenBody)
                 m1
                 (walkBody transformer elseBody)
                 m2
                 t
  walkStatement transformer (IfStmt pred thenBody m1 t) =
    transformStatement transformer $
      IfStmt (walkExp transformer pred)
             (walkBody transformer thenBody)
             m1
             t
  walkStatement transformer (Return0Stmt t) =
    transformStatement transformer $
      Return0Stmt t
  walkStatement transformer (Return1Stmt exp t) =
    transformStatement transformer $
      Return1Stmt (walkExp transformer exp) t
  walkStatement transformer (ReadStmt lexp t) =
    transformStatement transformer $
      ReadStmt (walkLExp transformer lexp) t
  walkStatement transformer (WriteStmt lexp t) =
    transformStatement transformer $
      WriteStmt (walkLExp transformer lexp) t
  walkStatement transformer (WhileStmt m1 pred body m2 t) =
    transformStatement transformer $
      WhileStmt m1
                (walkExp transformer pred)
                (walkBody transformer body)
                m2
                t

  walkBody :: ASTTransformer t -> Body t -> Body t
  walkBody transformer (Body decls stmts t) = (transformBody transformer) $ Body (walkDeclarations transformer decls) (walkStatements transformer stmts) t
  walkDeclarations :: ASTTransformer t -> Declarations t -> Declarations t
  walkDeclarations transformer decls = map (walkDeclaration transformer) decls
  walkExps :: ASTTransformer t -> Expressions t -> Expressions t
  walkExps transformer exps = map (walkExp transformer) exps
  walkStatements :: ASTTransformer t -> Statements t -> Statements t
  walkStatements transformer stmts = map (walkStatement transformer) stmts

  transformDeclarations' :: [ASTTransformer t] -> Declarations t -> Declarations t
  transformDeclarations' [] decls = decls
  transformDeclarations' (transformer:transformers) decls =
    let decls' = walkDeclarations transformer decls
    in transformDeclarations' transformers decls'

  transformers :: [ASTTransformer t]
  transformers = [arrayRefTransformer]

  transformDeclarations :: Declarations t -> Declarations t
  transformDeclarations decls = transformDeclarations' transformers decls