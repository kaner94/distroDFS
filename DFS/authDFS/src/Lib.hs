{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char -- This will be used to encrypt strings 😁
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.MongoDB                 (Action, Document, Value,
                                        access, allCollections, allDatabases, close, connect, delete,
                                        exclude, find, insert, findOne, host, insertMany,
                                        master, project, rest, select, sort,
                                        (=:))

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

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

type API = "users" :> Get '[JSON] [User]
		:<|> "token1" :> Get '[JSON] Token

main = do
  let s = "this is a string" 
  let x =  map ord s
  print (s)
  print (x)
  let z = map (+4) x
  print (z)
  let y = map chr z
  print (y)
  let a = map ord y
  let b = map (+(-4)) a 
  print (b)
  let c = map chr b
  print (c)



-- encrypt :: IO ()
-- encrypt inString = do
--   --let cipherString = [a..z]
--   --let s = s = (inString (map toUpper s)
--   print (ord inString)






startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
	:<|> return token1

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

keys :: Key
keys = Key 4

token1 :: Token
token1 = Token "THIS_IS_META_DATA" keys

-- runMongo functionToRun = do
--   pipe <- connect (host "127.0.0.1")
--   e <- access pipe maste "fileDB" functionToRun
--   print e
--   close pipe