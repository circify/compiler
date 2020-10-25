module AST.Zinc
  ( 
    
  ) where

data Literal = BoolLit Bool
             | StrLit String
             | IntLit IntLitBase String

data IntLitBase = Dec | Hex | Bin | Oct

data Block = Block [Stmt] (Maybe Expr)

data Expr = Ident String
          | LitExpr Literal
          | Cond Expr Expr Expr
          | BlockExpr Block

data Stmt = ExprStmt Expr
          | ForWhile String Bounds (Maybe Expr) Block
          | If Expr Block (Maybe Block)

data Bounds = Bounds Expr Expr