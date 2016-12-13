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

{-
1. Going to return files here
2. Going to post files too here
3. Going to send JSON to Caching Service

-}

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

data Key = Key
  { keyString :: Int }
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''Key)

type API = "postFile" :> ReqBody '[JSON] InFile :> Post '[JSON] ResponseData



startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = postFile

localKey :: Key
localKey = Key 4

encrypt :: String -> Int -> String
encrypt inString keyNum = do
  let intString = map ord inString
  let mappedIntString = map (+keyNum) intString -- Currently set to +4 testing purposes!
  let encString = map chr mappedIntString
  return encString!!0 -- This is necessary as map returns a [String], we just want the first element!


runMongo functionToRun = do
  pipe <-  connect (host "127.0.0.1")
  e <- access pipe master "fileDB" functionToRun
  print e
  close pipe


showCollections = runMongo allCollections

-- The >>= operator take the result of one action and feeds it as the argument to the next one
showFiles = runMongo $ find (select [] "files") >>= rest

-- The >>= operator take the result of one action and feeds it as the argument to the next one
showUsers = runMongo $ find (select [] "users") >>= rest

postFile :: InFile -> Handler ResponseData
postFile inFile = liftIO $ do
  let x = fileContents inFile
  let toPost = encrypt x (keyString localKey)
  print (inFile)
  print(toPost)
  let encFile = InFile toPost
  print(encFile)
  e <- insertFile $ ( toBSON $ encFile)
  return $ ResponseData (fileContents encFile)

insertFile :: Document -> IO ()
insertFile toInsert = runMongo $ insert "files" toInsert


