import System.Environment

main = do
  (source : destination : _) <- getArgs
  copyFile source destination

copyFile :: FilePath -> FilePath -> IO ()
copyFile source destination = do
  content <- readFile source
  writeFile destination content