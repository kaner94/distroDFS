{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Bson.Generic
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import System.IO
import Control.Monad.Trans (liftIO)
import Database.MongoDB 				(Action, Document, Value,
                                        access, allCollections, allDatabases, close, connect, delete,
                                        exclude, find, insert, findOne, host, insertMany,
                                        master, project, rest, select, sort,
                                        (=:))



data InFile = InFile
 	{ fileContents :: String }
 	deriving (Generic, FromBSON, ToBSON, FromJSON, ToJSON)

deriving instance FromBSON String
deriving instance ToBSON String 

data ResponseData = ResponseData
	{ response :: String }
	deriving (Generic)

instance FromJSON ResponseData
instance ToJSON ResponseData

data User = User 
	{ name :: String
	, age :: Int 
	, email :: String
	} deriving (Eq, Show, Generic, Read)


$(deriveJSON defaultOptions ''User)


-- instance ToJSON User
-- instance FromJSON User
-- instance FromJSON Message
-- instance ToJSON Message


type API = "postFile" :> ReqBody '[JSON] InFile :> Post '[JSON] ResponseData
		 

usersCollection :: [User]
usersCollection =
	[ User "Ryan Kane" 21 "ryan@kane.ie"
	, User "Neill Diamond" 68 "neill@diamond.hi"
	]

ryan :: User
ryan = User "Ryan Kane" 21 "ryan@kane.ie"

neill :: User
neill = User "Neill Diamond" 68 "neill@diamond.ie"


startApp :: IO ()
startApp = do
	putStrLn "Running on port 8080"
	run 8080 app
	
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  postFile
	-- echoMessage

-- echoMessage :: Server API
-- echoMessage = sendEcho where
-- 	sendEcho :: String -> Handler Message
--  	sendEcho s = return (Message (map toUpper s))

-- This function will be called by all further
-- functions accessing the DB.
-- Creates conncetion to fileDB
runMongo functionToRun = do
	pipe <-  connect (host "127.0.0.1")
	e <- access pipe master "fileDB" functionToRun
	print e
	close pipe

showCollections = runMongo allCollections

showFiles = runMongo $ find (select [] "files") >>= rest

insertFile :: Document -> IO ()
insertFile toInsert = runMongo $ insert "files" toInsert

-- postFile :: InFile -> Handler ResponseData
-- postFile inFile = liftIO $ do
-- 	e <- insertFile $ ( toBSON $ inFile)
-- 	return $ ResponseData (fileContents inFile)



postFile :: IO()
postFile = do
	handle <- openFile "text.txt" ReadMode
	contents <- hGetContents handle
	liftIO $ runMongo $ insert "files" contents

	withFile "text.txt" ReadMode (\handle -> do
		contents <- hGetContents handle
		runMongo $ insert "files" contents)




















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
