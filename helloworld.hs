import Data.Char
import System.Environment
import Control.Monad
import System.IO
import System.Directory
import Data.List

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
    putChar x
    myPutStr xs

myPrint :: (Show a) => a -> IO ()
myPrint = myPutStr . (++ "\n") . show

main = do
    handle <- openFile "todos.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contentes <- hGetContents handle
    let todoTasks = lines contentes
        numberedTodoTasks = zipWith (\index todo -> show index ++ " - " ++ todo)[0..] todoTasks
    putStrLn "Here are your TO'DOs"
    putStr $ unlines numberedTodoTasks
    putStrLn "Which one do you want to delete?"
    lineToDelete <- getLine
    let number = read lineToDelete
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle  
    hClose tempHandle
    removeFile "todos.txt"
    renameFile tempName "todos.txt"
