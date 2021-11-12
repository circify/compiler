module Parser.C where
import           Analyze.Static
import           Control.Monad.Reader
import           Language.C
import           Language.C.System.GCC
import qualified System.FilePath       as F
import           Util.Cfg              (Cfg)
import qualified Util.Cfg              as Cfg

-- | Given a filename, parse the C in that file
parseCE :: String -> Cfg (Either ParseError CTranslUnit)
parseCE file = do
  gccArgs <- asks Cfg._gccOptions
  liftIO $ parseCFile (newGCC "gcc")
                      Nothing
                      (gccArgs ++ ["-I" ++ F.takeDirectory file])
                      file

parseC :: String -> Cfg CTranslUnit
parseC file = do
  e <- parseCE file
  case e of
    Right ast -> liftIO $ slice ast >>= return
    Left err  -> error $ "parse error: " ++ show err
--  either (\e -> error $ "parse error: " ++ show e) id <$> parseCE file
