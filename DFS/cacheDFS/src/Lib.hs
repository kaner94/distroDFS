{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}

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
import Control.Monad.Trans (liftIO)
import Database.MongoDB 				(Action, Document, Value,
                                        access, allCollections, close, connect, delete,
                                        exclude, find, insert, findOne, host, insertMany,
                                        master, project, rest, select, sort,
                                        (=:))



data Message = Message
 	{ message :: String }
 	deriving (Generic)


data User = User 
	{ name :: String
	, age :: Int 
	, email :: String
	} deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
instance FromJSON Message
instance ToJSON Message


type API = "users" :> Get '[JSON] [User]
		 

usersCollection :: [User]
usersCollection =
	[ User "Ryan Kane" 21 "ryan@kane.ie"
	, User "Neill Diamond" 68 "neill@diamond.hi"
	]

startApp :: IO ()
startApp = do
	putStrLn "Running on port 8080"
	run 8080 app
	
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return usersCollection 

	-- echoMessage

-- echoMessage :: Server API
-- echoMessage = sendEcho where
-- 	sendEcho :: String -> Handler Message
--  	sendEcho s = return (Message (map toUpper s))


runMongo functionToRun = do
	pipe <-  connect (host "127.0.0.1")
	e <- access pipe master "fileDB" functionToRun
	print e
	close pipe





























































-- {-# LANGUAGE DataKinds       #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeOperators   #-}
-- {-# LANGUAGE DeriveGeneric   #-}


-- module Lib
--     ( startApp
--     ) where

-- import Data.Aeson
-- import Data.Aeson.TH
-- import Data.Char
-- import Data.List
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Servant
-- import GHC.Generics
-- import System.IO
-- import qualified Data.ByteString.Lazy as B


-- data Message = Message
-- 	{ message :: String }
--  	deriving (Read, Show)

-- instance FromJSON Message
-- instance ToJSON Message


-- -- messageFile :: FilePath
-- -- messageFile = "text.txt"

-- -- readMessage :: IO Message
-- -- readMessage = return (Message (B.readFile messageFile))


-- type API = "file" :> Get '[JSON] Message
-- 		-- "message" :> Capture "in" String :> Get '[JSON] Message 
-- 		-- :<|> 

-- -- inFile :: FilePath -> Handler SendFile
-- -- inFile = do
-- -- 	handle <- openFile "text.txt" ReadMode
-- -- 	sendFile <- hGetContents handle

-- -- testing :: IO ()
-- -- testing = do
-- -- 	inFile <- openFile "text.txt" ReadMode
-- -- 	contents <- hGetContents inFile
-- -- 	putStr contents
-- -- 	-- return (SendFile (contents))

-- startApp :: IO ()
-- startApp = do
-- 	-- handle <- openFile "text.txt" ReadMode
-- 	-- sendFile <- hGetContents handle
-- 	-- putStr sendFile
-- 	run 8080 app
	
-- app :: Application
-- app = serve api server

-- api :: Proxy API
-- api = Proxy

-- server :: Server API
-- server = return messages

-- messages :: [Message]
-- messages = [ toMessage ]

-- -- readMessage :: Server API
-- -- readMessage = sendEcho where
-- -- 	sendEcho :: String -> Handler Message
-- --  	sendEcho s = return (Message (map toUpper s))