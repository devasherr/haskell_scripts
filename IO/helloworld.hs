import Control.Monad
import Data.Char

-- main = do
--     colors <- forM [1,2,3,4] (\a -> do
--         putStrLn $ "which color for this number" ++ show a
--         color <- getLine
--         return color)
    
--     putStrLn "color picked are"
--     mapM putStrLn colors

-- main = interact isPalindorm

-- isPalindorm :: String -> String
-- isPalindorm = unlines . map (\xs -> if isPal xs then "palindrom" else "not palindrom") . lines

-- isPal :: String -> Bool
-- isPal x = x == reverse x



import System.IO
import Control.Monad.Cont (cont)
import Control.Exception (bracket)
import System.Directory (removeFile, renameFile)

-- main :: IO ()
-- main = do
--     handle <- openFile "test.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- main = do
--     withFile "test.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)

-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = bracket (openFile path mode) (\handle -> hClose handle) (\handle -> f handle)

-- main = do
--     contents <- readFile "test.txt"
--     appendFile "test.txt" (map toUpper contents)

-- time to make the good old TODO LIST :)
import System.Directory
import Data.List
import Control.Exception

main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    
    putStrLn "Todos: "
    mapM_ putStrLn numberedTasks
    putStrLn "Which on do you want to delete?"
    numberString <- getLine

    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
    