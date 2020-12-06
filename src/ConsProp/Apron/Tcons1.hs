module ConsProp.Apron.Tcons1 where
import           ConsProp.Apron.AbstractMonad
import           ConsProp.Apron.Lincons1
import           ConsProp.Apron.TexprOp
import           ConsProp.Apron.Var
import           ConsProp.Apron.Texpr1
import           Control.Monad              (void)
import           Control.Monad.State.Strict (liftIO)

data Tcons1 = Tcons1 Constyp Texpr1 Scalar

type Scalar = Int

tconsMake :: Constyp -> Texpr1 -> Scalar -> Abstract Tcons1
tconsMake c t s = do
  return (Tcons1 c t s)

-- True  -> The constraint holds
-- False -> The constraint does not hold
constypEval :: Constyp -> Var -> Scalar -> Bool
-- If the variable is unreachable, then obviously the result is still 
-- unreachable
constypEval _ Bottom _  = False
-- EQ is basically meet
constypEval CONS_EQ v s =
  case var of
    Bottom -> False
    _      -> True
  where var = absMeet v (Const s)
-- If we are not evaluating EQ sign, then all of the below would result in True
-- if the variable is Top
constypEval _ Top _        = True
constypEval op (Const c) s = 
  case op of
    CONS_DISEQ -> (c /= s)
    CONS_SUP   -> (c > s)
    CONS_SUPEQ -> (c >= s)
