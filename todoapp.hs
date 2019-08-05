import System.Environment   
import System.Directory  
import System.IO  
import Data.List

main = do
    (command: args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

dispatch:: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("view", view),
            ("remove", remove)]

add:: [String] -> IO ()
add [fileName, todo] = appendFile fileName (todo ++ "\n")

view:: [String] -> IO ()
view [fileName] = do
    content <- readFile fileName
    let todoTasks = lines content
        numberedTasks = zipWith (\index line -> show index ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName