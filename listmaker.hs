import System.IO ()
import System.Environment ( getArgs )

main = do
    -- Read command line arguments
    args <- getArgs
    let (fileName:_) = args

    -- read contents of file into a list
    contents <- readFile fileName
    -- print the list
    putStrLn "\nContents of file:"
    putStr contents

    -- make a list of contents of directory.txt
    let list = lines contents

    -- ask user for a choice
    putStrLn "\nEnter a choice"
    putStrLn "a Add a new entry"
    putStrLn "d Delete an entry"
    putStrLn "l Print the list"
    putStrLn "q Quit"
    choice <- getLine

    -- if a is chosen, add a new line to directory.txt
    case choice of
        "a" -> do
            putStrLn "Enter data you want to save:"
            newLine <- getLine
            appendFile fileName (newLine ++ "\n")
            main
        -- if r is chosen, ask user for a line number to remove
        "d" -> do
            putStrLn "Enter a line number to remove:"
            lineNumber <- getLine
            -- remove the line from the list
            let newList = removeLine (read lineNumber) list
            -- write the new list to directory.txt
            writeFile fileName (unlines newList)
            main
        -- if l is chosen, print the list
        "l" -> do
            putStrLn (unlines list)
            main
        -- if x is chosen, exit
        "q" -> return ()
        -- if anything else is chosen, print error message
        _ -> do
            putStrLn "Invalid choice"
            main

removeLine :: Int -> [String] -> [String]
removeLine _ [] = []
removeLine n (x:xs)
    | n == 1 = xs
    | otherwise = x : removeLine (n-1) xs

