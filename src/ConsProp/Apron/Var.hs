module ConsProp.Apron.Var where

data Var = Top | Bottom | Const Int
  deriving Show

isBottom :: Var -> Bool
isBottom Bottom = True
isBottom _      = False

isTop :: Var -> Bool
isTop Top = True
isTop _   = False

isLeq :: Var -> Var -> Bool
isLeq Bottom _ = True
isLeq _ Top    = True
-- Case (Top Top) and (Bottom Bottom) has been excluded
isLeq _ Bottom = False
isLeq Top _    = False
isLeq _ _      = True

isEq :: Var -> Var -> Bool
isEq Bottom Bottom       = True
isEq Top Top             = True
isEq (Const _) (Const _) = True
isEq _ _                 = False

absMeet :: Var -> Var -> Var
absMeet Bottom _            = Bottom
absMeet _ Bottom            = Bottom
absMeet Top a               = a
absMeet a Top               = a
absMeet (Const a) (Const b) =
  case (a == b) of
    True  -> Const a
    False -> Bottom

absJoin :: Var -> Var -> Var
absJoin Bottom a            = a
absJoin a Bottom            = a
absJoin Top _               = Top
absJoin _ Top               = Top
absJoin (Const a) (Const b) =
  case (a == b) of
    True  -> Const a
    False -> Top

-- No difference between widen and absJoin in this domain
widen :: Var -> Var -> Var
widen a b = absJoin a b

absAdd :: Var -> Var -> Var
absAdd Bottom _            = Bottom
absAdd _ Bottom            = Bottom
absAdd Top _               = Top
absAdd _ Top               = Top
absAdd (Const a) (Const b) = Const (a + b)

absSub :: Var -> Var -> Var
absSub Bottom _            = Bottom
absSub _ Bottom            = Bottom
absSub Top _               = Top
absSub _ Top               = Top
absSub (Const a) (Const b) = Const (a - b)

absMul :: Var -> Var -> Var
absMul Bottom _            = Bottom
absMul _ Bottom            = Bottom
absMul (Const 0) _         = Const 0
absMul _ (Const 0)         = Const 0
absMul Top _               = Top
absMul _ Top               = Top
absMul (Const a) (Const b) = Const (a * b)

absDiv :: Var -> Var -> Var
absDiv Bottom _            = Bottom
absDiv _ Bottom            = Bottom
absDiv _ (Const 0)         = error "Divided by 0"
absDiv (Const 0) _         = Const 0
absDiv Top _               = Top
absDiv _ Top               = Top
absDiv (Const a) (Const b) = Const (a `div` b)

absMod :: Var -> Var -> Var
absMod Bottom _            = Bottom
absMod _ Bottom            = Bottom
absMod _ (Const 0)         = error "Divided by 0"
absMod (Const 0) _         = Const 0
absMod Top _               = Top
absMod _ Top               = Top
absMod (Const a) (Const b) = Const (a `mod` b)

absNeg :: Var -> Var
absNeg (Const a) = Const (-a)
absNeg a         = a
