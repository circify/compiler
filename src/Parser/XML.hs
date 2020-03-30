module Parser.XML where
import           Control.Monad.State.Strict (forM_)
import qualified Data.ByteString            as B
import           System.Directory           (canonicalizePath,
                                             getCurrentDirectory)
import           System.FilePath            (isAbsolute, (</>))
import           System.FilePath.Find
import           Xeno.DOM

parseXMLDir :: String -> IO () --[Node]
parseXMLDir dir = do
  fs <- getSourceFiles dir [".xml"]
  forM_ fs parseXML
  error ""

parseXML :: String -> IO ()
parseXML filename = do
  contents <- B.readFile filename
  case parse contents of
    Right conts -> print conts
    Left err    -> print err

-- | Get all source files if dir0 is a directory, otherwise get file dir0
getSourceFiles :: FilePath -> [String] -> IO [FilePath]
getSourceFiles dir0 extns = do
  dir <- if isAbsolute dir0
         then return dir0
         else getCurrentDirectory >>= canonicalizePath . (</> dir0)
  getSourceFilesWithExtns extns dir

-- | Get all the source files given a list of extensions and absolute path
getSourceFilesWithExtns :: [String] -> FilePath -> IO [FilePath]
getSourceFilesWithExtns extns = find always (fileType ==? RegularFile &&? extnsPred)
  where extnsPred = foldl1 (||?) $ map (\extn -> extension ==? extn) extns

