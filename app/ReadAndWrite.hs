module ReadAndWrite where

import System.IO (readFile)


readFile_andProveIt :: IO ()
readFile_andProveIt = do
    content <- readFile "./wrote.md"
    putStr content

writeFile_destructive :: IO ()
writeFile_destructive = do
    let content = "This is the new new content for the file."
    writeFile "./wrote.md" content
    putStrLn "File written successfully."
