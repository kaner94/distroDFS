{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}


module Echo
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


data Message = Message
 	{ message :: String }
 	deriving (Generic)


instance FromJSON Message
instance ToJSON Message


type API = "message" :> Capture "in" String :> Get '[JSON] Message 
		 

startApp :: IO ()
startApp = run 8080 app
	
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = echoMessage

echoMessage :: Server API
echoMessage = sendEcho where
	sendEcho :: String -> Handler Message
 	sendEcho s = return (Message (map toUpper s))






