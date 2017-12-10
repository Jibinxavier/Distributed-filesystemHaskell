
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
import qualified FilesystemAPI   as FSA
import           FilesystemAPIServer 
import           Datatypes 
import           EncryptionAPI

type TAPI = "getTransId"               :> ReqBody '[JSON] Message1 :> Post '[JSON] ResponseData -- should start the transaction 
          :<|> "uploadToTransaction"   :> ReqBody '[JSON] TransactionContents :> Post '[JSON] Bool -- uploads will be stored in the transaction server
          :<|> "commit"                :> ReqBody '[JSON] Message :> Post '[JSON] Bool
          :<|> "abort"                 :> ReqBody '[JSON] Message :> Post '[JSON] Bool
          :<|> "readyToCommit"         :> ReqBody '[JSON] Message :> Post '[JSON] Bool
  
 
startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = FSA.withLogging $ \ aplogger -> do
  port <- FSA.transPorStr
  FSA.warnLog "Starting transaction service"

  let settings = setPort ((read $ port):: Int) $ setLogger aplogger defaultSettings
  runSettings settings app 
  
app :: Application
app = serve api server

api :: Proxy TAPI
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server TAPI
server = getTransId 
    :<|> uploadToTransaction   
    :<|> commit  
    :<|> abort 
    :<|> readyToCommit

  where
    ------ Dont know how to do an empty post request in haskell
    getTransId :: Message1 ->  Handler ResponseData
    getTransId (Message1 ticket) = liftIO $ do
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      FSA.warnLog $ "New transaction" 
      newId <- getNewTrId
      let encnewId = myEncryptAES (aesPad seshkey) (newId)
      FSA.withMongoDbConnection $ upsert  (select ["transid" =: newId] "Transaction_RECORD") $ toBSON $ Transaction newId [] []
       -- update the transaction with empty list

      return $ ResponseData encnewId
 
    uploadToTransaction :: TransactionContents -> Handler Bool
    uploadToTransaction mg@(TransactionContents _ _ ticket)   = liftIO $ do
      FSA.warnLog $ "Uploading a modification to the transaction server"
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let (TransactionContents transID modification@(TChanges(FInfoTransfer fp dir fileid _ _ _)_)_) = decryptTransactionContents seshkey mg 
      
      let path =dir++fileid
      
      FSA.withMongoDbConnection $ do
        docs <- find (select ["transid" =: transID] "Transaction_RECORD") >>= FSA.drainCursor
        let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) docs 
        case contents of
          [Transaction _ prevmods prevRTC] -> liftIO $ do -- RTC ready to commit
          -- records every change in the transaction in newMods, and newTc is to keep track of FS that arent ready                                    
            let newMods =prevmods ++ [modification]
            let newTC  = prevRTC++[path]     --to commit      

  
            FSA.withMongoDbConnection $ upsert  (select ["transid" =: transID] "Transaction_RECORD") $ toBSON $ Transaction transID newMods newTC
            return True
          [] ->    return False --- transaction doesnt exist
    
 
    sendChangesToFS ::  String-> TChanges -> IO ()
    sendChangesToFS trid1 (TChanges (FInfoTransfer filepath dir fileid ipadr portadr _ ) contents) = liftIO $ do
       let path =dir++fileid
       let toFS =TransactionInfoTransfer trid1 fileid contents
       systemHost_ <- FSA.defaultHost
       res <-  FSA.myrestfullCall (uploadToshadow toFS) ((read portadr) :: Int) systemHost_
       case res of
        Left err -> do
          
          FSA.warnLog $ "ERROR in sending changes to fileservers. TRANSACTION canceled" ++ show err
        Right (a) -> do 
          FSA.warnLog $ "Success: sending changes to fileservers" ++ show a
    
    
  -- pushing changes to the FS
    commit :: Message -> Handler Bool
    commit (Message enctid ticket) =liftIO $   do
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let tid = myDecryptAES (aesPad seshkey) enctid
      FSA.warnLog $ "Committing modifications to the fileservers"
      FSA.withMongoDbConnection $ do
        docs <- find (select ["transid" =: tid] "Transaction_RECORD") >>= FSA.drainCursor
        let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) docs 
        case (contents )of
          [] -> return False 
          [Transaction id mods _] -> liftIO $ do 
            FSA.warnLog $ "Committing modifications to the fileservers"
            
            foldM (\ a chg -> sendChangesToFS tid  chg) () mods
            return True
          
          ------- code to contact the fs servers
        -------- I'm not checking for errors here

 
    
    abort :: Message -> Handler Bool
    abort (Message enctid ticket) =liftIO $ do
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let tid = myDecryptAES (aesPad seshkey) enctid
      FSA.warnLog $ "Aborting transaction"
      FSA.withMongoDbConnection $ delete  (select ["transid" =: tid] "Transaction_RECORD")   
      port <- FSA.dirServPort
      systemHost_ <- FSA.defaultHost
      FSA.myrestfullCall (abortDirChanges $ Message1 tid) ((read port) :: Int) systemHost_
      return True
   
    
    
    -- starts from the begin and as it traverses it also checks if the all fileservers said ready to commit
    -- it presumes filename is in the list
    checkFSstatus:: [String] -> [String]-> String -> [String]
    checkFSstatus (x:xs) rest filename=
      case (x==filename) of
        True -> rest ++ xs
        False -> checkFSstatus xs (rest++[x]) filename
    checkFSstatus [] rest _ = rest
    --- Signal to all the fileservers to update their database a
    sendCommitSigToFS :: [TChanges] -> String -> IO ()
    sendCommitSigToFS ((TChanges (FInfoTransfer _ _ _ ipadr portadr _ ) contents):xs) transId = do
      -- have to pass something because api will be looking for string
      systemHost_ <- FSA.defaultHost
      FSA.myrestfullCall (updateRealDB $ transId) ((read portadr) :: Int) systemHost_

      sendCommitSigToFS xs transId

    sendCommitSigToFS [] _= do
       FSA.warnLog $ "Commit message sent to every fs"


    readyToCommit:: Message -> Handler Bool
    readyToCommit (Message transId fileid) = liftIO $ do
     FSA.warnLog $ "Ready to commit "++transId
     FSA.withMongoDbConnection $ do
        docs <- find (select ["transid" =: transId] "Transaction_RECORD") >>= FSA.drainCursor
        let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) docs 
        case contents of        
          [] -> return False --- incorrect transaction
          [Transaction _ mods toCommit] -> liftIO $ do
            let status= checkFSstatus toCommit [] fileid
            case status of
              [] -> do --- all the fileservers have indicated that they can make changes to the database
                port <- FSA.dirServPort
                systemHost_ <- FSA.defaultHost 
                FSA.myrestfullCall (commitDirChanges $ Message1 transId) ((read port) :: Int)  systemHost_
                sendCommitSigToFS mods transId --  
                FSA.withMongoDbConnection $ delete (select ["transid" =: transId] "Transaction_RECORD") 
                FSA.warnLog $ "Commit message sent to directory service"

                return True  
              newTC -> liftIO $ do -- newTC new to commit
                FSA.withMongoDbConnection $ upsert  (select ["transid" =: transId] "Transaction_RECORD") $ toBSON $ Transaction transId mods newTC
                return True ------ acknowledgement back to the server

 




-- stores running counter of the ids
getNewTrId :: IO String
getNewTrId = do 
  FSA.withMongoDbConnection $ do
          let key ="trans" :: String  -- hardcoding the key , didnt find anyway around it
          docs <- find (select ["id22" =: key] "TrasactionIdCounter_RECORD") >>= FSA.drainCursor
          let counters =   catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TrCounter) docs -- take 1 ensures that there is just one
          case counters of
            ((TrCounter  _ val):_) ->   liftIO $ do
              let oldId= read val :: Int 
              let id =oldId+1
              let idStr= show id
              FSA.withMongoDbConnection $ upsert  (select ["id22" =: key] "TrasactionIdCounter_RECORD") $ toBSON $ TrCounter key idStr
              return idStr

            [] ->  liftIO $ do
              FSA.withMongoDbConnection $ upsert  (select ["id22" =: key] "TrasactionIdCounter_RECORD") $ toBSON $ TrCounter key "0"
              return $ "0"
 