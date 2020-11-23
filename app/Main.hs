module Main where

import           SubCmd                         ( runCmd )
import           Options                        ( parseCmd )
import qualified Util.Cfg                      as Cfg
import           Util.Log                       ( evalLog )
import           Util.Show                      ( pShow )

main :: IO ()
main = do
  cmd <- parseCmd
  putStrLn $ "Cmd: " ++ pShow cmd
  cfg <- Cfg.setFromEnv Cfg.defaultCfgState
  Cfg.evalCfg (evalLog $ runCmd cmd) cfg
