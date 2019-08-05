import System.Environment
import qualified Data.ByteString as S 
import System.IO  
import System.IO.Error

main = do
  (source : destination : _) <- getArgs
  copyFile source destination

copyFile :: FilePath -> FilePath -> IO ()
copyFile source destination = do
  content <- S.readFile source
  S.writeFile destination content
