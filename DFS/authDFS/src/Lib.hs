{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char -- This will be used to encrypt strings 😁
import Data.String
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
  handle <- openFile "text.txt" ReadMode
  inFile <- hGetContents handle
  keyTest <- getKey
  let x = encrypt inFile keyTest
  print(x)

  let y = decrypt x keyTest
  print(y)

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
getKey = randomRIO(1,25)

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