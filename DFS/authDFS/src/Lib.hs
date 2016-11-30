{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
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
	,	key1 :: String
	}


$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Token)


type API = "users" :> Get '[JSON] [User]
		:<|> "token1" :> Get '[JSON] Token

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

token1 :: Token
token1 = Token "THIS_IS_META_DATA" "1332kglnz09"

runMongo functionToRun = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe maste "fileDB" functionToRun
  print e
  close pipe