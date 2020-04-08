module Static.ARM where
import           Xeno.DOM

analyze :: Node -> IO ()
analyze node = do
  print node
  print "OK"

