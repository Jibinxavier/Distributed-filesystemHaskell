{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
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
    ( someFunc

    ) where
import           Control.Concurrent           (forkIO, threadDelay)
import           System.IO
import           Control.Monad                 
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Bson.Generic 
import           Distribution.PackageDescription.TH
import           Git.Embed

import           Network.HTTP.Client          (defaultManagerSettings,newManager)

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Console.ANSI
import           System.Environment
import qualified FilesystemAPI as FSA  
import           FilesystemAPIClient 
import           Data.Time.Clock
import qualified Data.List                    as DL
import           Database.MongoDB       
import           Data.Maybe
import           GHC.Generics
import           Data.Text                    (pack, unpack)
import           Datatypes 
import           EncryptionAPI
import           Helpers        
import           Data.List.Split
import           Data.Char 
import           Datatypes

import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
-- write to db
-- sleep and read from db
-- 

type API1 =  "lockAvailable"         :> ReqBody '[JSON] LockTransfer  :> Post '[JSON] Bool
 

startApp :: String -> IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp port = FSA.withLogging $ \ aplogger -> do 
  FSA.warnLog $ "Starting client " ++port
   

  let settings =setPort (read port) $ setLogger aplogger defaultSettings
  forkIO $ menu  
  runSettings settings app


app :: Application
app = serve api server

api :: Proxy API1
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API1
server = lockAvailable
  where
    lockAvailable :: LockTransfer -> Handler Bool
    lockAvailable lockdetails@(LockTransfer filepath _ ) =  liftIO $ do 
      FSA.withMongoDbConnection  $ upsert (select ["filepathL" =: filepath] "LockAvailability_RECORD") $ toBSON $ lockdetails
      return True


{-
    Checks every 10 seconds if the client is allocated a lock
-}

waitOnLock :: String ->  IO (Bool)
waitOnLock  filepath = liftIO $ do
  putStrLn $ "waiting for lock" ++ filepath
 
  docs <- FSA.withMongoDbConnection  $ find  (select ["filepathL" =: filepath] "LockAvailability_RECORD")  >>= FSA.drainCursor 
  let  lock= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe LockTransfer) docs 
  case lock of 
    ([LockTransfer _ True]) -> do 
      putStrLn "CLIENT: LOCK AVAILABLE YAY!!"
      return True
    ([LockTransfer _ False] ) -> do 
      putStrLn "CLIENT: Going to sleep as lock is not available"
      threadDelay $ 10 * 1000000
      waitOnLock filepath 
    otherwise  -> do 
      putStrLn $ "CLIENT: Lock details are not available locally" ++ show lock

      return False
  
  
-- Locking file
---------------------------------------------------
 
doFileLock :: String-> String -> IO (Bool)
doFileLock fpath usern= do
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> do 
      lockport <- FSA.lockPortStr
      clientport <- FSA.getClientPort
      let encFpath = myEncryptAES (aesPad seshkey) (fpath)
      let encUname = myEncryptAES (aesPad seshkey) (usern)
      let encClientport = myEncryptAES (aesPad seshkey) (clientport)
      
      resp <- FSA.myrestfullCall  (lock $ Message4  encClientport encFpath encUname ticket) (read lockport) FSA.systemHost
      case resp of
        Left err -> do 
          putStrLn $ "failed to lock ... " ++  show err
          return False
          -- queue and lock
        Right ([_, True]) -> do
          putStrLn "Got the lock"
          return True
        Right ([True, _]) -> do 
          putStrLn "On queue now"
          --FSA.withMongoDbConnection $ insert "LockAvailability_RECORD" $ toBSON $ LockTransfer fpath False
          FSA.withMongoDbConnection  $ upsert (select ["filepathL" =: fpath] "LockAvailability_RECORD") $ toBSON $ LockTransfer fpath False
          waitOnLock fpath  
    (Nothing) -> do 
      putStrLn $ "Expired token . Sigin in again.  " 
      return False
  

doFileUnLock :: String-> String -> IO ()
doFileUnLock fpath usern= do
  lockport <- FSA.lockPortStr
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> do 
      let encFpath = myEncryptAES (aesPad seshkey) (fpath)
      let encUname = myEncryptAES (aesPad seshkey) (usern)
      -- sesh to decrpt message sent back
      restfullCall (unlock  $ Message3 encFpath encUname ticket) FSA.lockIP (Just lockport)seshkey
    (Nothing) -> putStrLn $ " Expired token  .Sigin in again. " 

doIsLocked :: String  ->  IO ()
doIsLocked fpath  = do
  lockport <- FSA.lockPortStr
  restfullCall (islocked $ Just fpath) FSA.lockIP (Just lockport) $  seshNop
---------------------------------
-- Directory services
---------------------------------

 

  
doListDirs :: String -> IO ()
doListDirs usern=  do 
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> do 
      dirP <-FSA.dirServPort
      -- sesh to decrpt message sent back
      restfullCall (listdirs $ Just ticket) FSA.dirHost (Just dirP) seshkey
    (Nothing) -> putStrLn $ "Expired token . Sigin in again.  " 
   

doLSFileServerContents :: String  -> String -> IO ()
doLSFileServerContents dir usern=do
  dirP <-FSA.dirServPort
  restfullCallMsg1WithEnc listfscontents dir usern FSA.dirHost (Just dirP)



doFileSearch :: String -> String -> String -> IO ()
doFileSearch dir fname usern = do 
  dirP <-FSA.dirServPort
  restfullCallMsg3WithEnc  filesearch dir fname usern FSA.dirHost (Just dirP)
 

-----------------------------
-- Transaction service
-----------------------------
-- client can only do one transaction at a time     
-- Checks the database if there is an transaction running, it would abort and continue with new transaction  
doGetTransId :: String-> IO ()
doGetTransId usern=  do
  trsport <-FSA.transPorStr
  res <-  mydoCalMsg1WithEnc getTransId usern ((read $ trsport:: Int))
  case res of
    Nothing ->   putStrLn $ "get file call to fileserver  failed with error: "  
    Just (ResponseData enctrId) -> do 
 

      authInfo <- getAuthClientInfo usern
      case authInfo of 
        (Just (ticket,seshkey) ) -> do
          let trId =  myDecryptAES (aesPad seshkey)  (enctrId) 
           
           -- getting previous transaction id of the client
          clientTrans <- getLocalTrId usern
          case clientTrans of 
            [LocalTransInfo _  prevId] -> liftIO $ do  -- abort and update
                putStrLn $ "Aborting old transaction and starting new " 
      
                restfullCallMsg1WithEnc abort prevId usern FSA.transIP (Just trsport) -- notifying transaction server to abort 
                FSA.withMongoDbConnection $ upsert (select ["key1" =: usern] "Transaction_RECORD") $ toBSON $ LocalTransInfo usern trId -- store the transaction id
            [] ->  putStrLn $ "Starting new transaction " ++trId
            
          FSA.withMongoDbConnection $ upsert (select ["key1" =: usern] "Transaction_RECORD") $ toBSON $ LocalTransInfo usern trId -- store the transaction id
 
        (Nothing) -> putStrLn $ " Expired token  .Sigin in again. " 
      
 
     
doCommit :: String -> IO ()
doCommit usern = do 
  trsport <-FSA.transPorStr
  localTransactionInfo <- getLocalTrId usern
  case localTransactionInfo of        
    [ LocalTransInfo _ trId] -> liftIO $ do  
      restfullCallMsg1WithEnc commit trId usern FSA.transIP (Just trsport)
      unlockLockedFiles trId usern
      clearTransaction usern-- clearing after commiting the transaction
    [] -> putStrLn "No transactions to  commit"

doAbort :: String -> IO ()
doAbort usern  = do 
  trsport <-FSA.transPorStr
  localTransactionInfo <- getLocalTrId usern
  case localTransactionInfo of 
    [LocalTransInfo _ trId] -> liftIO $ do   
      restfullCallMsg1WithEnc abort trId usern FSA.transIP (Just trsport)
      unlockLockedFiles trId usern
      clearTransaction usern-- clearing after aborting the transaction
    [] -> putStrLn "No transactions to  abort"
-- localfilePath : file path in the client
-- dir           : fileserver name
-- fname         : filename 

doWriteWithTransaction:: String-> String -> String -> IO ()
doWriteWithTransaction remoteFPath usern newcontent  = do
  let remotedir =head $ splitOn "/" remoteFPath
      fname = last $ splitOn "/" remoteFPath  
      localfilePath = "./" ++ fname
  
  trsport <-FSA.transPorStr
  localTransactionInfo <- getLocalTrId  usern
  --  Client will just specify where it wants to store the file 
  -- transaction has to figure out the file info and update directory info  
 
  status <- isFileLocked remoteFPath
  case status of  --- if the file is locked it cannot be added to the transaction
    (False) -> do
      case localTransactionInfo of  -- get local transaction info
        [LocalTransInfo _ trId] -> liftIO $ do     
          existingContent <- readFile localfilePath
          let contents = existingContent ++ newcontent
       
          doFileLock remoteFPath usern -- lock the file
          appendToLockedFiles remoteFPath trId -- list of locked files which the client keeps a record of
          dirport <- FSA.dirServPort
          res <- mydoCalMsg4WithEnc uploadToShadowDir remotedir fname trId usern ((read $ dirport):: Int) decryptFInfoTransfer -- uploading info to shadow directory 

          case res of
            Nothing -> putStrLn $ "Upload to transaction failed"  
            Just (a) ->   do 
              case a of 
                ([fileinfotransfer @(FInfoTransfer _ _ fileid _ _ _ )]) -> do
                  let filecontents=FileContents fileid  contents ""
                
                  let transactionContent=TransactionContents trId (TChanges fileinfotransfer filecontents ) ""
                  
                  --- encrypting transaction information before uploading to the transaction  server
                  authInfo <- getAuthClientInfo usern
                  case authInfo of 
                    (Just (ticket,seshkey) ) -> do 
                      let msg = encryptTransactionContents transactionContent seshkey ticket
                      
                      restfullCall (uploadToTransaction $ msg) FSA.transIP  (Just trsport) $  seshNop
                    (Nothing) -> putStrLn $ " Expired token  .Sigin in again. " 
                [] -> putStrLn "doWriteWithTransaction: Error getting fileinfo "


        
          
        [] -> putStrLn "No ongoing transaction"

    (True) -> putStrLn "File is locked"


---------------------------------

{-
  doWriteFileworkflow
  1.check if the file is locked
  2.if not send the file metadata to the directory service
  3.Lock the file
  4.Update file local copy 
  5.Encrypt the contents and send it to the fileserver 
-}


doWriteFile::  String  -> String ->  String -> IO ()
doWriteFile  remoteFPath usern newcontent = do   -- call to the directory server saying this file has been updated 
  
  let remotedir =head $ splitOn "/" remoteFPath
      fname = last $ splitOn "/" remoteFPath  
      localfilePath = "./" ++ fname
  
  state <- doFileLock remoteFPath usern 
  case state of 
    (True) -> do 
      
        dirport <- FSA.dirServPort
        res <- mydoCalMsg3WithEnc updateUploadInfo remotedir fname usern ((read dirport):: Int) decryptFInfoTransfer
        case res of
          Nothing -> putStrLn $ "Upload file failed call failed " 
          (Just a) ->   do 
            case a of 
              [(fileinfotransfer@(FInfoTransfer _ _ fileid h p ts ))] -> do   
                case  p =="none" of
                  (True)->  putStrLn "Upload failed : No fileservers available."
                  (False)-> do
                    -- lock file before storing
                    putStrLn $ "Recieved file lock" 
                    -- update local file and push the changes up
                    

                    putStrLn $ "CLIENT: Primary copy host"++ show h ++ "  port "++ show p
                    appendFile localfilePath $ newcontent
                    contents <- readFile localfilePath

                    putStrLn  contents
                    authInfo <- getAuthClientInfo usern
                    case authInfo of 
                      (Just (ticket,seshkey) ) -> do 
                        let msg = encryptFileContents  (FileContents fileid contents "") seshkey ticket -- encrypted message
                        restfullCall (upload  msg) (Just "systemHost") (Just p)  seshkey -- uploading file
                        putStrLn "after uploading"
                        doFileUnLock remoteFPath usern
                        putStrLn "after unlock"
                        -- store the metadata about the file
                        -- let fpath =  usern ++ remoteFPath -- this is to allow multiple users to use the same database
                        updateLocalMeta remoteFPath $ FInfo remoteFPath remotedir fileid ts
                        putStrLn "file unlocked "
                      (Nothing) -> putStrLn $ "Expired token . Sigin in again.  "  
              [] -> putStrLn "Upload file : Error getting fileinfo from directory service"

         
    (False) -> putStrLn "File is locked"

-- client



-- filepath :- id fileserver'
-- dir has to be the name
-- fname    : filename
 
displayFile :: String -> IO ()
displayFile filepath = do
  putStrLn $ "Printing contents of the file"
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  print contents
  hClose handle   

 

doReadFile :: String -> String-> IO ()
doReadFile remoteFPath usern = do 
  -- talk to the directory service to get the file details
  let remotedir =head $ splitOn "/" remoteFPath
      fname = last $ splitOn "/" remoteFPath
  dirport <- FSA.dirServPort
  res <- mydoCalMsg3WithEnc filesearch remotedir fname usern ((read dirport):: Int) decryptFInfoTransfer
  case res of
    Nothing ->  putStrLn $ "download call failed" 
    (Just fileinfo@resp) ->   do 
      case resp of
        [FInfoTransfer remotefilepath dirname fileid ipadr portadr servTm1 ] -> do 
          putStrLn $ portadr ++ "file id "++ fileid
           
          status <- isDated remotefilepath servTm1  --check with timestamp in the database 
          case status of
            True ->  getFileFromFS  fileinfo usern -- it also updates local file metadata
            False -> putStrLn "You have most up to date  version" 
          displayFile fname
        [] -> putStrLn "CLIENT: This file is not in the fileserver directory" 
        
      
 

-- gets the public key of the auth server and encrypts message and sends it over 
doSignup:: String -> String -> IO ()
doSignup userN pass =  do
  authport <-FSA.authPortStr
  resp <- FSA.myrestfullCall (loadPublicKey) ((read authport):: Int) FSA.systemHost
  case resp of
    Left err -> do
      putStrLn $ "failed to get public key... " ++  show err
    Right ((ResponseData a):(ResponseData b):(ResponseData c):rest) -> do
      let authKey = toPublicKey (PubKeyInfo a b c)
      cryptPass <- encryptPass authKey pass
      putStrLn "got the public key!"
      putStrLn "Sent encrypted username and password to authserver"
      restfullCall (signup $ UserInfo userN cryptPass) FSA.authIP (Just authport) $  seshNop
      


doLogin:: String -> String-> IO ()
doLogin userN pass  = do
  authport <-FSA.authPortStr
  resp <- FSA.myrestfullCall (loadPublicKey) ((read authport ):: Int) FSA.systemHost
  case resp of
    Left err -> do
      putStrLn "failed to get public key..."
    Right ((ResponseData a):(ResponseData b):(ResponseData c):rest) -> do
      let authKey = toPublicKey (PubKeyInfo a b c)
      cryptPass <- encryptPass authKey pass
      
     
      myrestfullCall2  (storeClientAuthInfo userN pass) (login $ UserInfo userN cryptPass) ((read authport):: Int)
     
      

-- First we invoke the options on the entry point.
someFunc :: IO ()
someFunc = do 
  args <- getArgs
  
  case args of
    [clientport] -> do
      
       
      setEnv "CLIENT_PORT" clientport
      -- setEnv "MONGODB_PORT" mongoport
      -- setEnv "MONGODB_IP" "127.0.0.1"
      -- setEnv "MONGODB_DATABASE" "USEHASKELLDB"
      let key = "nee":: String
      
      -- FSA.withMongoDbConnection $ insert "TEST1"   ["owner" =: key]
      -- FSA.withMongoDbConnection $ do
      --   docs <- findOne (select ["owner" =: key] "TEST1")
        
      --   case docs of
      --     (Just _) -> liftIO $ putStrLn "got it"
      --     (Nothing) -> liftIO $ putStrLn "nothing"
      -- putStrLn "here"
      startApp clientport
    _ -> putStrLn "Bad parameters. Port number for client expected"
  



menu = do
  contents <- getLine 
  if DL.isPrefixOf "login" contents
    then do
      let cmds =  splitOn " " contents
      --"User name" password"
      doLogin (cmds !! 1) (cmds !! 2)
  else if DL.isPrefixOf  "signup" contents
    then do
      let cmds =  splitOn " " contents
      --"User name" password"
      doSignup  (cmds !! 1) (cmds !! 2)
  else if DL.isPrefixOf  "readfile" contents
    then do
      let cmds =  splitOn " " contents
      -- "remote dir/fname (filepath)"    "username"
      doReadFile  (cmds !! 1) (cmds !! 2) 
  else if DL.isPrefixOf  "write" contents
    then do
      let cmds =  splitOn " " contents
      -- "remote dir/fname (filepath)" "user name" "content to add"
     
      doWriteFile  (cmds !! 1) (cmds !! 2) (cmds !! 3)  
  else if DL.isPrefixOf  "lockfile" contents
    then do
      let cmds =  splitOn " " contents
      -- "remote dir/filename"   "user name"
      doFileLock  (cmds !! 1) (cmds !! 2) 
      return ()
  else if DL.isPrefixOf  "unlockfile" contents
    then do
      let cmds =  splitOn " " contents
      -- "remote dir/filename"   "user name"
      doFileUnLock  (cmds !! 1) (cmds !! 2) 
  else if DL.isPrefixOf  "listdirs" contents
    then do
      let cmds =  splitOn " " contents
      --   "user name"
      doListDirs  (cmds !! 1) 
  else if DL.isPrefixOf  "lsdircontents" contents
    then do
      let cmds =  splitOn " " contents
      --   "remote dir/filename" "user name"
      doLSFileServerContents  (cmds !! 1)    (cmds !! 2) 
  else if DL.isPrefixOf  "filesearch" contents
    then do
      let cmds =  splitOn " " contents
      --  "remote dir"  "remote dir/filename"   "user name"
      doFileSearch  (cmds !! 1) (cmds !! 2) (cmds !! 3) 

  else if DL.isPrefixOf  "startTrans" contents
    then do
      let cmds =  splitOn " " contents
      --    "user name"
      doGetTransId  (cmds !! 1) 
  else if DL.isPrefixOf  "commit" contents
    then do
      let cmds =  splitOn " " contents
      --    "user name"
      doCommit  (cmds !! 1) 
  else if DL.isPrefixOf  "abort" contents
    then do
      let cmds =  splitOn " " contents
      --    "user name"
      doAbort  (cmds !! 1) 
  else if DL.isPrefixOf  "writeT" contents
    then do
      let cmds =  splitOn " " contents
     --  "remote dir/fname (filepath)" "user name" "content to add"
      doWriteWithTransaction  (cmds !! 1) (cmds !! 2) (cmds !! 3) 
  else
    putStrLn $"no command specified"
  menu


unlockLockedFiles :: String  -> String -> IO() 
unlockLockedFiles  tid  usern= liftIO $ do
   docs <- FSA.withMongoDbConnection $ find (select ["tid5" =: tid] "LockedFiles_RECORD") >>= FSA.drainCursor
   let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe LockedFiles) docs 
   case contents of 
    [] -> return ()
    [LockedFiles _ files] -> do -- adding to existing transaction 
      foldM (\ a filepath -> doFileUnLock filepath usern) () files
      FSA.withMongoDbConnection $ delete (select ["tid5" =: tid] "LockedFiles_RECORD")  
 