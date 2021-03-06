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
import Data.Char -- This will be used to encrypt strings 😁
import Data.String
import Data.Bson.Generic
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import System.Random
import GHC.Generics
import Control.Monad.Trans (liftIO)
import Database.MongoDB                 (Action, Document, Value,
                                        access, allCollections, allDatabases, close, connect, delete,
                                        exclude, find, insert, findOne, host, insertMany,
                                        master, project, rest, select, sort,
                                        (=:))


data InFile = InFile
  { fileContents :: String }
  deriving (Generic, FromBSON, ToBSON, FromJSON, ToJSON, Show, Read)

deriving instance FromBSON String
deriving instance ToBSON String 

data ResponseData = ResponseData
  { response :: String }
  deriving (Generic)

instance FromJSON ResponseData
instance ToJSON ResponseData


data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data TestUser = TestUser
  { name :: String
  , password :: String
  } deriving(Generic, Eq, Show, Read, FromBSON, ToBSON)

data Token = Token
	{   metaData :: String
	,	  key1 :: Key
	}
  deriving (Show, Read)

data Key = Key
  { keyString :: Int }
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Token)
$(deriveJSON defaultOptions ''Key)
$(deriveJSON defaultOptions ''TestUser)


type API = "users" :> Get '[JSON] [User]
		:<|> "token1" :> Get '[JSON] Token
    :<|> "postFile" :> ReqBody '[JSON] InFile :> Post '[JSON] ResponseData
    :<|> "addUser" :> ReqBody '[JSON] TestUser :> Post '[JSON] ResponseData
    :<|> "getToken" :> ReqBody '[JSON] TestUser :> Post '[JSON] ResponseData


main = do
  handle <- openFile "text.txt" ReadMode
  inFile <- hGetContents handle
  keyTest <- getKey
  print ("This is keyTest ")
  print (keyTest)

  let inttt = keyTest
  print ("\n\nThis is Inttt: ")
  print (inttt)

  let test = Key inttt
  print("\n\nThis is the key: ")
  print (test)

  let x = encrypt inFile keyTest
  print(x)

  let y = decrypt x keyTest
  putStrLn(y)


  -- let testingEncrypt = encrypt inFile
  -- print(testingEncrypt)

encrypt :: String -> Int -> String
encrypt inString keyNum = do
  let intString = map ord inString
  let mappedIntString = map (+keyNum) intString -- Currently set to +5 testing purposes!
  let encString = map chr mappedIntString
  return encString!!0 -- This is necessary as map returns a [String], we just want the first element!

decrypt:: String -> Int -> String
decrypt inString keyNum = do
  let intString = map ord inString
  let mappedIntString = map (+(-keyNum)) intString
  let decString = map chr mappedIntString
  return decString!!0 -- This is necessary as map returns a [String], we just want the first element!


getKey :: IO Int
getKey = randomRIO(1,5)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
	:<|> return token1
  :<|> postFile
  :<|> addUser
  :<|> getToken

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

localKey :: Key
localKey = Key 4

token1 :: Token
token1 = Token "THIS_IS_META_DATA" localKey

runMongo functionToRun = do
  pipe <-  connect (host "127.0.0.1")
  e <- access pipe master "fileDB" functionToRun
  print e
  close pipe

showCollections = runMongo allCollections

showFiles = runMongo $ find (select [] "files") >>= rest

showUsers = runMongo $ find (select [] "users") >>= rest

insertFile :: Document -> IO ()
insertFile toInsert = runMongo $ insert "files" toInsert

insertUser :: Document -> IO ()
insertUser toInsert = runMongo $ insert "users" toInsert

getToken :: TestUser -> Handler ResponseData
getToken inUser = liftIO $ do
  let findName = name inUser
  let pass = password inUser
  let encPass = encrypt pass (keyString localKey)
  user <- runMongo $ findOne $ select ["name" =: findName, "password" =: encPass] "users"
  return $ ResponseData (metaData token1)

postFile :: InFile -> Handler ResponseData
postFile inFile = liftIO $ do
  let x = fileContents inFile
  print (inFile)
  e <- insertFile $ ( toBSON $ inFile)
  return $ ResponseData (fileContents inFile)

addUser :: TestUser -> Handler ResponseData
addUser inUser = liftIO $ do
  let x = name inUser
  let y = password inUser
  let encY = encrypt y (keyString localKey)
  print(y)
  print(encY)
  let toPost = TestUser x encY
  e <- insertUser $ ( toBSON $ toPost)
  return $ ResponseData (name toPost)

-- Gonna need a function that'll take in a passowrd/username and ensure
-- its correct, and then give that person a token :woo:

-- runMongo functionToRun = do
--   pipe <- connect (host "127.0.0.1")
--   e <- access pipe maste "fileDB" functionToRun
--   print e
--   close pipe