{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module FilesystemAPI where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
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
import           Codec.Crypto.RSA
import qualified System.Random  as R

import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8 as C
import           Codec.Binary.UTF8.String as S
import           Crypto.Random.DRBG
import           Crypto.Cipher.AES
import           System.Process
import           System.IO

import           Datatypes 
import           EncryptionAPI
-- Note that in this version of the project, I have moved the REST API into a shared library called use-haskell-api
-- This library is imported here in order that the HackageAPI type is available to create the REST service of that
-- type. Note that there is no advantage in doing this if you are only building a servant REST service, but if you are
-- creating a corresponding REST client, then following this architectural pattern simplifies development considerably.

-- The relevant code is thus commented out here and the use-haskell-api library content is used instead


-------------------------communication between services-----------------

defaultHost :: IO String
defaultHost = do
 (_, Just hout, _, _) <-createProcess (shell "/sbin/ip route|awk '/default/ { print $3 }'" ){ std_out = CreatePipe }

 getHost <- hGetContents hout
 return $ filter (/= '\n') getHost

systemHost="127.0.0.1"
dirServPort= "8070"
dirPort= Just dirServPort
dirHost = Just systemHost

transPorStr ="8078"
transPort= Just transPorStr
transIP = Just systemHost

 
lockPortStr ="8077"
lockPort= Just lockPortStr
lockIP = Just systemHost

authPortStr ="8076"
authPort= Just authPortStr
authIP = Just systemHost
--- they will be communicating on the same ip but different ports
mydoCall f port= (SC.runClientM f =<< envFileApi port) 

envFileApi :: Int -> IO SC.ClientEnv
envFileApi  port=do
     manager <- newManager defaultManagerSettings
     return (SC.ClientEnv manager (SC.BaseUrl SC.Http systemHost port ""))
 
 
------------------------communication between services------------------
-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data

fileserverIp :: IO String
fileserverIp = defEnv "FILESERVER_IP" id "F1" True

fileserverPort :: IO String
fileserverPort = defEnv "FILESERVER_Port" id "8080" True

fileserverName :: IO String
fileserverName = defEnv "FILESERVER_Name" id "F1" True

ipadddress :: IO String
ipadddress = defEnv "ipaddress" id "F1" True


fileserverType :: IO String
fileserverType = defEnv "FILESERVER_Type" id "8080" True

mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def

type API = "load_environment_variables" :> QueryParam "name" String :> Get '[JSON] ResponseData
      :<|> "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "storeMessage"               :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
      :<|> "searchMessage"              :> QueryParam "name" String :> Get '[JSON] [Message]
      :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData
      :<|> "upload"                     :> ReqBody '[JSON] FileContents  :> Post '[JSON] Bool
      :<|> "download"                   :> ReqBody '[JSON] Message   :> Get '[JSON] [FInfo]

      :<|> "lock"                       :> ReqBody '[JSON] Message3  :> Post '[JSON] Bool
      :<|> "unlock"                     :> ReqBody '[JSON] Message3  :> Post '[JSON] Bool
      :<|> "islocked"                   :> QueryParam "filename" String :> Get '[JSON] Bool


      

      :<|> "listdirs"                   :> QueryParam "ticket" String :> Get '[JSON] [String] 
      :<|> "listfscontents"             :> ReqBody '[JSON] Message  :> Post '[JSON]  [FSContents] -- change to listdircontents
      :<|> "filesearch"                 :> ReqBody '[JSON] Message3  :> Post '[JSON] [FInfoTransfer]
      :<|> "updateUploadInfo"           :> ReqBody '[JSON] Message3  :> Post '[JSON] [FInfoTransfer]

      

      
      :<|> "getTransId"                 :> ReqBody '[JSON] Message1 :> Post '[JSON] ResponseData
      :<|> "uploadToTransaction"        :> ReqBody '[JSON] TransactionContents :> Post '[JSON] Bool 
      :<|> "commit"                     :> ReqBody '[JSON] Message :> Post '[JSON] Bool
      :<|> "abort"                      :> ReqBody '[JSON] Message :> Post '[JSON] Bool
      :<|> "uploadToShadowDir"          :> ReqBody '[JSON] Message4 :> Post '[JSON] [FInfoTransfer] -- [] used as error
             
      :<|> "login"                      :> ReqBody '[JSON] UserInfo  :> Post '[JSON] [ResponseData]
      :<|> "signup"                     :> ReqBody '[JSON] UserInfo  :> Post '[JSON]  ResponseData
      :<|> "loadPublicKey"              :> Get '[JSON] [ResponseData]
      :<|> "getFile"                    :> ReqBody '[JSON] Message  :> Get '[JSON] [FileContents]

type FSAPI =      "add_dir"             :> ReqBody '[JSON] Message3  :> Post '[JSON] Bool -- so that the file server can register it self
           :<|>   "heartbeat"           :> ReqBody '[JSON] Message  :> Post '[JSON] Bool 
           :<|>   "getAllReplicas"      :> QueryParam "name" String  :> Get '[JSON] [FServer] 
           :<|>   "broadcastedUpload"   :> ReqBody '[JSON] FileContents   :> Post '[JSON] Bool
           :<|>   "readyToCommit"       :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
type TSAPI = "uploadToshadow"        :> ReqBody '[JSON] TransactionInfoTransfer  :> Post '[JSON] Bool
           :<|> "updateRealDB"       :> ReqBody '[JSON] String        :> Post '[JSON] Bool -- commit changes from shadow space to actual thing
           :<|> "commitDirChanges"   :> ReqBody '[JSON] Message1  :> Post '[JSON] Bool
           :<|> "abortDirChanges"    :> ReqBody '[JSON] Message1  :> Post '[JSON] Bool
             