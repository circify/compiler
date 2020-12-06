module ConsProp.Apron.TexprOp where

data OpType = ADD_OP
            | SUB_OP
            | MUL_OP
            | DIV_OP
            | MOD_OP
            | POW_OP
            | NEG_OP
            | CAST_OP
            | SQRT_OP
  deriving (Eq, Show)

data RoundingType = ROUND_REAL
                  | ROUND_INT
                  | ROUND_SINGLE
                  | ROUND_DOUBLE
                  | ROUND_EXTENDED
                  | ROUND_QUAD
                  | ROUND_DO_NOT_USE_0
  deriving (Eq, Show)

data RoundingDir = ROUND_NEAREST
                 | ROUND_ZERO
                 | ROUND_UP
                 | ROUND_DOWN
                 | ROUND_ALL
                 | ROUND_DO_NOT_USE_1
  deriving (Eq, Show)
