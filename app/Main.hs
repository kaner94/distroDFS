module Main where

{-
	Auth Server for DFS. 
	Lib will receive key from File System, give token and simulate encryption 
-}



import Lib
import Database.MongoDB

main :: IO ()
main = do
  putStrLn "hello world"


runMongo authDB functionToRun = do
	pipe <- runIOE $ connect(host "127.0.0.1")
	e <- access pipe master (pack authDB) functionToRun
	close pipe