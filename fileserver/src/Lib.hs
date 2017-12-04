
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                 
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Crypto.BCrypt
import qualified Data.ByteString.Char8        as BS
import           FilesystemAPI  
import           FilesystemAPIServer  
import           Datatypes 
import           EncryptionAPI
 
noHeartBeats=20
type API1 = "upload"                      :> ReqBody '[JSON] FileContents   :> Post '[JSON] Bool
        :<|> "broadcastedUpload"          :> ReqBody '[JSON] FileContents   :> Post '[JSON] Bool
        :<|> "getFile"                    :> ReqBody '[JSON] Message        :> Get '[JSON] [FileContents]
        :<|> "uploadToshadow"             :> ReqBody '[JSON] TransactionInfoTransfer   :> Post '[JSON] Bool
        :<|> "updateRealDB"               :> ReqBody '[JSON] String         :> Post '[JSON] Bool

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
    warnLog "Starting filesystem"  
    fport <- fileserverPort --- environment variable in docker-compose up
    let fSort=(read fport) :: Int
    fileserverHost <- defaultHost 
    let settings = setPort fSort $ setLogger aplogger defaultSettings
    myapp<- app
    forkIO $ sendHeartBeat noHeartBeats fileserverHost fport
    runSettings settings myapp