module ConsProp.Apron.Lincons1 where

data Constyp = CONS_SUP
             | CONS_SUPEQ
             | CONS_EQ
             | CONS_DISEQ
  deriving (Eq, Show)
