{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}


module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import System.IO
import qualified Data.ByteString.Lazy as B


data Message = Message
	{ message :: String }
 	deriving (Read, Show)

instance FromJSON Message
instance ToJSON Message


-- messageFile :: FilePath
-- messageFile = "text.txt"

-- readMessage :: IO Message
-- readMessage = return (Message (B.readFile messageFile))


type API = "file" :> Get '[JSON] Message
		-- "message" :> Capture "in" String :> Get '[JSON] Message 
		-- :<|> 

-- inFile :: FilePath -> Handler SendFile
-- inFile = do
-- 	handle <- openFile "text.txt" ReadMode
-- 	sendFile <- hGetContents handle

-- testing :: IO ()
-- testing = do
-- 	inFile <- openFile "text.txt" ReadMode
-- 	contents <- hGetContents inFile
-- 	putStr contents
-- 	-- return (SendFile (contents))

startApp :: IO ()
startApp = do
	-- handle <- openFile "text.txt" ReadMode
	-- sendFile <- hGetContents handle
	-- putStr sendFile
	run 8080 app
	
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return messages

messages :: [Message]
messages = [ toMessage ]

-- readMessage :: Server API
-- readMessage = sendEcho where
-- 	sendEcho :: String -> Handler Message
--  	sendEcho s = return (Message (map toUpper s))
