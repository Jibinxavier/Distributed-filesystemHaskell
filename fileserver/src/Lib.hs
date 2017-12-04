
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
import qualified FilesystemAPI as FSA  
import           FilesystemAPIServer  
import           Datatypes 
import           EncryptionAPI   (myDecryptAES, myDecryptAES,aesPad,sharedSeed, encryptFileContents, decryptFileContents)
noHeartBeats=20


type API1 = "upload"                      :> ReqBody '[JSON] FileContents   :> Post '[JSON] Bool
        :<|> "broadcastedUpload"          :> ReqBody '[JSON] FileContents   :> Post '[JSON] Bool
        :<|> "getFile"                    :> ReqBody '[JSON] Message        :> Get '[JSON] [FileContents]
        :<|> "uploadToshadow"             :> ReqBody '[JSON] TransactionInfoTransfer   :> Post '[JSON] Bool
        :<|> "updateRealDB"               :> ReqBody '[JSON] String         :> Post '[JSON] Bool
        

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = FSA.withLogging $ \ aplogger -> do

  FSA.warnLog "Starting filesystem"  
  fport <- FSA.fileserverPort --- environment variable in docker-compose up
  let fSort=(read fport) :: Int
  fileserverHost <- FSA.defaultHost 
  let settings = setPort fSort $ setLogger aplogger defaultSettings
  myapp<- app
  forkIO $ sendHeartBeat noHeartBeats fileserverHost fport
  runSettings settings myapp

broadcast ::FileContents -> IO ()
broadcast filecontents =do
  dirname <- FSA.fileserverName
  
  res <- FSA.mydoCall (getAllReplicas ( Just dirname))  ((read FSA.dirServPort)::Int)
  case res of
    Left err -> do
      FSA.warnLog $ " getAllReplicas failed with error: " ++ show err
    Right (replicas) ->liftIO $ do 
      sendBroadcast replicas filecontents
       -- sending copies,  
      FSA.warnLog "Sucessfully sent"  

sendBroadcast :: [FServer]-> FileContents -> IO ()
sendBroadcast  [] _=  return () 
sendBroadcast  ((FServer  _ port ):xs) msg  =   do 
  FSA.mydoCall (FilesystemAPIServer.broadcastedUpload $  msg) ((read port)::Int) 
  sendBroadcast xs msg

sendHeartBeat :: Int ->  String ->String  -> IO ()
sendHeartBeat delay host port= do
  FSA.warnLog $ "Sending heart beat to directory." 
  
  FSA.mydoCall (heartbeat $  Message host port) ((read FSA.dirServPort)::Int )
  threadDelay $ delay * 1000000
  sendHeartBeat delay host port -- tail recursion

registerWDir :: IO () 
registerWDir= do
   -----------------------------------------------------------------------------------------
  dirname <- FSA.fileserverName
  fport <- FSA.fileserverPort
  fileserverHost <- FSA.defaultHost 
  FSA.warnLog "Registering with directory service at port "    
  res <- FSA.mydoCall (add_dir $  Message3 fileserverHost fport dirname ) ((read FSA.dirServPort)::Int )
  case res of
    Left err -> do
      FSA.warnLog $ " register failed with error: " ++ show err
    Right (a) -> do 
      FSA.warnLog "Sucessfully registered with directory service"  
  ---------------------------------------------------------------------------------------------
app :: IO  Application
app =  do 
  registerWDir
  return $ serve api server

api :: Proxy API1
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API1
server = upload
    :<|> broadcastedUpload
    :<|> getFile
    :<|> uploadToshadow
    :<|> updateRealDB



  where
    -- if it is a replica it will recieve the copy
    broadcastedUpload :: FileContents -> Handler Bool -- received  by a replica
    broadcastedUpload msg@(FileContents fileid _ _)= liftIO $ do
      FSA.warnLog $ " Storing file  " ++ fileid ++ "." 
      FSA.withMongoDbConnection $ upsert (select ["fileP" =: fileid] "Files_RECORD") $ toBSON $ msg
      return True
    -- if it is the primary copy 
    upload :: FileContents -> Handler Bool -- primary copy received 
    upload mg@(FileContents _ _ ticket)= liftIO $ do
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let filecontents@(FileContents fileid _ _)=decryptFileContents mg seshkey
      FSA.warnLog $ " Storing file  " ++ fileid ++ "."
      broadcast filecontents 
      FSA.withMongoDbConnection $ upsert (select ["fileP" =: fileid] "Files_RECORD") $ toBSON $ filecontents
      return True


    getFile ::Message -> Handler [FileContents]
    getFile   (Message encfileid ticket) = liftIO $ do
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let fileid= myDecryptAES (aesPad seshkey) (encfileid)
      FSA.warnLog $ "Searching the file" ++ fileid ++ "."
      FSA.withMongoDbConnection $ do
        docs <- find (select ["fileP" =: fileid] "Files_RECORD") >>= FSA.drainCursor
        let docs'=catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileContents) docs
    
        return $ map (\ b -> encryptFileContents b seshkey ""  ) docs'



        
    uploadToshadow :: TransactionInfoTransfer -> Handler Bool
    uploadToshadow msg@(TransactionInfoTransfer trid id filecontents )= liftIO $ do
      FSA.warnLog $ " Storing file in shadow space " ++ trid ++ "." -- key has to be transaction id.
     
      FSA.withMongoDbConnection $ do
        docs <- find  (select ["trid3" =: trid] "Shadow_RECORD") >>= FSA.drainCursor
        let  docs'=  take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServShadowChanges) docs
        case docs' of 
          [FileServShadowChanges _ oldFileContents] -> liftIO $  do  
            let newList= oldFileContents++[filecontents]  
            FSA.withMongoDbConnection $ upsert (select ["trid3" =: trid] "Shadow_RECORD") $ toBSON $ FileServShadowChanges trid newList
           
          [] ->liftIO $ do
            FSA.withMongoDbConnection $ upsert (select ["trid3" =: trid] "Shadow_RECORD") $ toBSON $ FileServShadowChanges trid [filecontents]

      dirname <- FSA.fileserverName
      let fileid= dirname++id
      let msgToTrans = Message trid fileid -- 
      FSA.mydoCall   (readyToCommit $  msgToTrans) ((read FSA.transPorStr)::Int) 
      return True  
     
    updateRealDB :: String -> Handler Bool
    updateRealDB transId =  liftIO $ do
      FSA.warnLog $ " Updating the real database" ++ "."  
      --- extract changes from the shadow space and update the actual db
      docs <- FSA.withMongoDbConnection $ find  (select ["trid3" =: transId] "Shadow_RECORD") >>= FSA.drainCursor
      let contents=  take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileServShadowChanges) docs
      case contents of
        [(FileServShadowChanges _ changes)] -> do
          foldM (\ a filecontents@(FileContents fileid _ _) -> do
            broadcast filecontents
            FSA.withMongoDbConnection $ upsert (select ["fileP" =: fileid] "Files_RECORD") $ toBSON filecontents) () changes
          
          FSA.withMongoDbConnection $ delete (select [] "Shadow_RECORD")   --- clean the shadow space (might receive multiple updaterealdb)
          return True
        [] ->return True
     
 
     
      
