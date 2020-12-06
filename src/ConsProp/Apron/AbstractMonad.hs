{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ConsProp.Apron.AbstractMonad where
import qualified Control.Monad.Fail         as Fail
import           Control.Monad.State.Strict
import           Data.List                  (find)

data Domain = Constants
            | Intervals
            | Polyhedra
            | Octagons
            | Zonotopes
            deriving (Eq, Ord, Show)

type VarName = String

data Environment = Env { variable :: [VarName]
                       }

data AbstractState = AbstractState { unDomain      :: Domain
                                   , unEnvironment :: Environment
                                   }

allocEnv :: [String] -> Abstract Environment
allocEnv intVars = do
  return (Env intVars)

envInsertVar :: Environment -> VarName -> Environment
envInsertVar (Env v) var = Env (v ++ [var])

-- | Monad for a given analysis
newtype Abstract a = Abstract { unAbstractState :: StateT AbstractState IO a }
    deriving (Functor, Applicative, Monad, MonadState AbstractState, MonadIO, Fail.MonadFail)

-- | Evaluate an abstract action with a given configurations.
evalAbstract :: AbstractState -> Abstract a -> IO a
evalAbstract aState act = evalStateT (unAbstractState act) aState

defaultState :: AbstractState
defaultState = AbstractState Constants undefined

getDomain :: Abstract Domain
getDomain = gets unDomain

getEnvironment :: Abstract Environment
getEnvironment = gets unEnvironment

initVar :: VarName -> Abstract ()
initVar v = do
  s0 <- get
  put $ s0 { unEnvironment = envInsertVar (unEnvironment s0) v }

findVar :: VarName -> Abstract Bool
findVar v = do
  (Env vs) <- gets unEnvironment
  case (find (==v) vs) of
    Nothing -> return False
    _       -> return True

initAbstractState :: Domain
                 -> [VarName]
                 -> [VarName]
                 -> Abstract ()
initAbstractState domain intVars realVars = do
  env <- allocEnv intVars
  case domain of
    Constants -> return ()
    _         -> error "This tool only supports constant domain, please refer to the main tool for other domains"
  forM_ intVars initVar
  s0 <- get
  put $ s0 { unDomain      = domain
           , unEnvironment = env
           }
