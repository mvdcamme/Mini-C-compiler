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

  -- Attempt 2
  class HasExpTransformer t where
    doTransformExp :: Expression t -> Expression t
  newtype ExpressionIdTransformer t       = ExpressionIdTransformer (Expression t)
  newtype ExpressionChangerTransformer t  = ExpressionChangerTransformer (Expression t)
  instance HasExpTransformer (ExpressionIdTransformer t) where
    doTransformExp exp = exp
  instance HasExpTransformer (ExpressionChangerTransformer t) where
    doTransformExp (BinaryExp _ left _ t) = LengthExp left t
    doTransformExp exp = exp
  walkExp2' :: HasExpTransformer t => Expression t -> Expression t
  walkExp2' exp = doTransformExp exp

  --Attempt 3
  class HasTransformer t where
    transformBody2 :: Body t -> Body t
    transformDeclaration2 :: Declaration t -> Declaration t
    transformExp2 :: Expression t -> Expression t
    transformLExp2 :: LeftExpression t -> LeftExpression t
    transformPExp2 :: PointerExpression t -> PointerExpression t
    transformStatement2 :: Statement t -> Statement t

  newtype ASTTransformerID = ASTTransformerID ()
  instance HasTransformer (ASTTransformerID) where
    transformBody2 = id
    transformDeclaration2 = id
    transformExp2 = id
    transformLExp2 = id
    transformPExp2 = id
    transformStatement2 = id

  newtype ASTTransformerChanger = ASTTransformerChanger ()
  instance HasTransformer (ASTTransformerChanger) where
    transformBody2 = id
    transformDeclaration2 = id
    transformExp2 (BinaryExp _ left _ t) = LengthExp left t
    transformExp2 exp = exp
    transformLExp2 = id
    transformPExp2 = id
    transformStatement2 = id

  walkDeclaration2 :: HasTransformer t => Declaration t -> Declaration t
  walkDeclaration2 decl@(VarDeclaration typ name t) =
    transformDeclaration2 decl
  walkDeclaration2 decl@(FunDeclaration typ name decls body t) =
    transformDeclaration2 $ FunDeclaration typ name (walkDeclarations2 decls) (walkBody2 body) t

  walkExp2 :: HasTransformer t => Expression t -> Expression t
  walkExp2 (LeftExp lexp t) =
    transformExp2 $ LeftExp (walkLExp2 lexp) t
  walkExp2 (BinaryExp binop left right t) =
    transformExp2 $ BinaryExp binop (walkExp2 left) (walkExp2 right) t

  walkLExp2 :: HasTransformer t => LeftExpression t -> LeftExpression t
  walkLExp2 lexp@(VariableRefExp name t) =
    transformLExp2 lexp
  walkLExp2 (ArrayRefExp lexp exp t) =
    transformLExp2 $ ArrayRefExp (walkLExp2 lexp) (walkExp2 exp) t
  walkLExp2 (DerefExp pexp t) =
    transformLExp2 $ DerefExp (walkPExp2 pexp) t

  walkPExp2 :: HasTransformer t => PointerExpression t -> PointerExpression t
  walkPExp2 (PointerExp lexp exp t) =
    transformPExp2 $ PointerExp (walkLExp2 lexp) (walkExp2 exp) t

  walkStatement2 :: HasTransformer t => Statement t -> Statement t
  walkStatement2 statement = statement -- TODO

  walkBody2 :: HasTransformer t => Body t -> Body t
  walkBody2 (Body decls stmts t) = (transformBody2) $ Body (walkDeclarations2 decls) (walkStatements2 stmts) t
  walkDeclarations2 :: HasTransformer t => Declarations t -> Declarations t
  walkDeclarations2 decls = map (walkDeclaration2) decls
  walkStatements2 :: HasTransformer t => Statements t -> Statements t
  walkStatements2 stmts = map (walkStatement2) stmts

  -- idTransformer :: ASTTransformer t
  -- idTransformer = ASTTransformer id id id id id id

  -- walkDeclaration :: ASTTransformer t -> Declaration t -> Declaration t
  -- walkDeclaration transformer decl@(VarDeclaration typ name t) =
  --   transformDeclaration transformer $ decl
  -- walkDeclaration transformer decl@(FunDeclaration typ name decls body t) =
  --   transformDeclaration transformer $ FunDeclaration typ name (walkDeclarations transformer decls) (walkBody transformer body) t

  -- walkExp :: ASTTransformer t -> Expression t -> Expression t
  -- walkExp transformer (LeftExp lexp t) =
  --   (transformExp transformer) $ LeftExp (walkLExp transformer lexp) t
  -- walkExp transformer (BinaryExp binop left right t) =
  --   (transformExp transformer) $ BinaryExp binop (walkExp transformer left) (walkExp transformer right) t

  -- walkLExp :: ASTTransformer t -> LeftExpression t -> LeftExpression t
  -- walkLExp transformer lexp@(VariableRefExp name t) =
  --   transformLExp transformer lexp
  -- walkLExp transformer (ArrayRefExp lexp exp t) =
  --   transformLExp transformer $ ArrayRefExp (walkLExp transformer lexp) (walkExp transformer exp) t
  -- walkLExp transformer (DerefExp pexp t) =
  --   transformLExp transformer $ DerefExp (walkPExp transformer pexp) t

  -- walkPExp :: ASTTransformer t -> PointerExpression t -> PointerExpression t
  -- walkPExp transformer (PointerExp lexp exp t) =
  --   transformPExp transformer $ PointerExp (walkLExp transformer lexp) (walkExp transformer exp) t


  -- walkStatement :: ASTTransformer t -> Statement t -> Statement t
  -- walkStatement transformer statement = statement -- TODO

  -- walkBody :: ASTTransformer t -> Body t -> Body t
  -- walkBody transformer (Body decls stmts t) = (transformBody transformer) $ Body (walkDeclarations transformer decls) (walkStatements transformer stmts) t
  -- walkDeclarations :: ASTTransformer t -> Declarations t -> Declarations t
  -- walkDeclarations transformer decls = map (walkDeclaration transformer) decls
  -- walkStatements :: ASTTransformer t -> Statements t -> Statements t
  -- walkStatements transformer stmts = map (walkStatement transformer) stmts

--   class TraverseBody t where
--     traverseBody :: Body t -> Body t
--   class TraverseDeclaration t where
--     traverseDeclaration :: Declaration t -> Declaration t
--   class TraverseExp t where
--     traverseExp :: Expression t -> Expression t
--   class TraverseLExp t where
--     traverseLExp :: LeftExpression t -> LeftExpression t
--   class TraversePExp t where
--     traversePExp :: PointerExpression t -> PointerExpression t
--   class TraverseStatement t where
--     traverseStatement :: Statement t -> Statement t

--   newtype TraverseBodyId t            = TraverseBodyId (Body t)
--   newtype TraverseDeclarationId t     = TraverseDeclarationId (Declaration t)
--   newtype TraverseExpId t             = TraverseExpId (Expression t)
--   newtype TraverseLExpId t            = TraverseLExpId (LeftExpression t)
--   newtype TraversePExpId t            = TraversePExpId (PointerExpression t)
--   newtype TraverseStatementId t       = TraverseStatementId (Statement t)

--   instance TraverseLExp (TraverseLExpId t) where
--     traverseLExp lexp = lexp
--   instance (TraverseLExp t) => TraverseExp (TraverseExpId t) where
--     traverseExp (LeftExp lexp t) = (LeftExp (traverseLExp lexp) t)
--     traverseExp exp = exp

--   traverseASTLeftExp :: LeftExpression t -> LeftExpression t
--   traverseASTLeftExp (ArrayRefExp lexp exp t) = DerefExp (PointerExp lexp exp t) t
--   traverseASTLeftExp lexp = lexp

--   -- traverseASTExp :: Expression t -> Expression t
--   -- traverseASTExp (LeftExp lexp t) = LeftExp (traverseASTLeftExp lexp) t
--   -- traverseASTExp (BinaryExp binop left right t) =
--   --   BinaryExp binop (traverseASTExpression left) (traverseASTExpression right) t
--   -- traverseASTExp (UnaryExp unop arg t) =
--   --   UnaryExp unop (traverseASTExpression arg) t
--   -- traverseASTExp (UnaryModifyingExp unop lexp t) =
--   --   UnaryModifyingExp unop (traverseASTLeftExp lexp) t
--   -- traverseASTExp (FunctionAppExp name args t) =
--   --   FunctionAppExp name (map traverseASTExpression args) t
--   -- traverseASTExp (FunctionAppWithTypedExp name args t) =
--   --   FunctionAppWithTypedExp name (map (\tuple -> (traverseASTExpression $ fst tuple, snd tuple)) args) t
--   -- traverseASTExp (LengthExp exp t) =
--   --   LengthExp (traverseASTExpression exp) t
--   -- traverseASTExp (AddressOf lexp t) =
--   --   AddressOf (traverseASTLeftExpression exp) t
--   -- traverseASTExp (NumberExp i t) = NumberExp i t
--   -- traverseASTExp (QCharExp c t) = QCharExp c t

--   -- traverseASTArrayRef :: Statements t -> Statements t
--   -- traverseASTArrayRef statements = map traverseASTArrayRefStatement statements