
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
import           Data.Time.Clock               
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
import           System.Random
import qualified FilesystemAPI as FSA    
import           Datatypes 
import           EncryptionAPI
type DIRAPI = "listdirs"           :> QueryParam "ticket" String :>  Get '[JSON] [String] 
        :<|> "listfscontents"      :> ReqBody '[JSON] Message  :> Post '[JSON]  [FSContents] -- change to listdircontents
        :<|> "filesearch"          :> ReqBody '[JSON] Message3  :> Post '[JSON] [FInfoTransfer]
        :<|> "updateUploadInfo"    :> ReqBody '[JSON] Message3  :> Post '[JSON] [FInfoTransfer] -- updating after uploading
        :<|> "add_dir"             :> ReqBody '[JSON] Message3  :> Post '[JSON] Bool --    --  
        :<|> "uploadToShadowDir"   :> ReqBody '[JSON] Message4  :> Post '[JSON] [FInfoTransfer]
        :<|> "commitDirChanges"    :> ReqBody '[JSON] Message1  :> Post '[JSON] Bool 
        :<|> "abortDirChanges"     :> ReqBody '[JSON] Message1  :> Post '[JSON] Bool 
        :<|> "heartbeat"           :> ReqBody '[JSON] Message  :> Post '[JSON] Bool -- ip and port of the fileserver
        :<|> "getAllReplicas"      :> QueryParam "name" String  :> Get '[JSON] [FServer]  
---------------helper functions------------------


heartbeatThresh= 120

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = FSA.withLogging $ \ aplogger -> do

  FSA.warnLog "Starting directory service"
  port <- FSA.dirServPort
  let settings = setPort  ((read $ port):: Int) $ setLogger aplogger defaultSettings
 
  runSettings settings app 
  
app :: Application
app = serve api server

api :: Proxy DIRAPI
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server DIRAPI
server = listdirs 
    :<|> listfscontents   
    :<|> filesearch  -- return fileid and port ip --- read
    :<|> updateUploadInfo  --  kind of write
    :<|> add_dir 
    :<|> uploadToShadowDir
    :<|> commitDirChanges
    :<|> abortDirChanges
    :<|> heartbeat
    :<|> getAllReplicas
  where

    listdirs ::  Maybe String -> Handler [String]
    listdirs (Just encticket)= liftIO $ do
       
      let seshkey  = myDecryptAES (aesPad sharedSeed) encticket
      FSA.warnLog $ "Searching for list of directories: "   
      FSA.withMongoDbConnection $ do
        docs <- find (select [] "Directory_RECORD") >>= FSA.drainCursor
        let docs' =catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FServerMap) docs
        let dirs = map dirName docs' 
        return $ map (myEncryptAES (aesPad seshkey)) dirs -- encrypt directories

    listdirs Nothing=return ([] ::[String])
      
    listfscontents :: Message-> Handler [FSContents] 
    listfscontents msg@(Message encdir ticket) =  liftIO $ do
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let dir = myDecryptAES (aesPad seshkey) encdir
      FSA.warnLog $ "Searching for files in directory: "
      let key= dir
      FSA.withMongoDbConnection $ do
        docs <- find (select ["dirN" =: key] "FSFileMap_RECORD") >>= FSA.drainCursor------- neefd to change

        let fscont=catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FSContents) docs
        
        return $ encryptFSContents fscont [] seshkey  
    

    filesearch :: Message3 -> Handler [FInfoTransfer]
    filesearch msg@(Message3 encdir encfname ticket) =  liftIO $ do 
      let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      let (dir ,fname) =decryptMessage3  msg
      FSA.warnLog $ "Searching for file: "
      let path= dir++fname  
      removeDeadNodes dir  ------------------------- clear dead nodes
     
      docs <- FSA.withMongoDbConnection $ find (select ["filepath" =: path] "Files_RECORD") >>= FSA.drainCursor
      let docs'= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FInfo) docs 
      case docs' of
        ([FInfo _ _ fileid timestamp]) ->    do  
          (ip, port) <- pickReplica dir -- get ip and port of a directory
          
          let res= encryptFInfoTransfer (FInfoTransfer path dir fileid ip port timestamp) seshkey
          return  [res ]
        ([])->   return $ ([] :: [FInfoTransfer])
       
    

   
       
    
    --- If a file is uploaded, information about that file is updated
    updateUploadInfo :: Message3 -> Handler [FInfoTransfer]  
    updateUploadInfo msg@(Message3 encdir encfname ticket) =  liftIO $ do
       let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
       let (dir ,fname) =decryptMessage3  msg
       dirEx <- dirExists dir
       case dirEx of
          (True) -> liftIO $ do
            removeDeadNodes dir  ------------------------- clear dead nodes
            let path= dir++fname  
            toInsert@(FInfo _ _ fileid timestamp) <- createFileInfo dir fname -- might be uncessary information to 
            FSA.withMongoDbConnection $ upsert  (select ["filepath" =: path] "Files_RECORD") $ toBSON $ toInsert  -- file mapping
            ---
            appendIfNew dir fname -- appends to list of files this directory holds
            (ip,port) <- getPCAddr dir -- primary copy

            let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
            let toSendback = encryptFInfoTransfer (FInfoTransfer path dir fileid ip port timestamp) seshkey
            return [toSendback]   --- return file reference
              
          (False) -> return $ ([] :: [FInfoTransfer])
    

      -- notice the structure of the first line of code: fn1 >>= fn2
      -- An alternative way to write this is:
      --        a <- fn1
      --        b <- fn2 a
      -- but for short IO function chains, it can be easier to use >>= to chain them.
      --
      -- In fact, the code above can be compressed further, although it is a question of style as to which is
      -- preferable:
      --
    --- could be changed stores running counter of the ids


    

    add_dir :: Message3 -> Handler Bool
    add_dir (Message3 ip port dirname )  = liftIO $ do
      FSA.warnLog $ "Storing new file server to  "  ++ dirname++ "  directory"
      docs <- FSA.withMongoDbConnection $ find (select ["dirName" =: dirname] "Directory_RECORD") >>= FSA.drainCursor
      let dirs =   take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FServerMap) docs
      case dirs of
        [FServerMap _ primary fileservers ] ->  liftIO $ do  
          FSA.warnLog $ "IS NEW DIRECTORY     "++ ip++ port ++ show (  isNewDir ip port  primary fileservers)
          case (isNewDir ip port  primary fileservers) of 
            (True) -> do 

              case primary of 
                (Nothing) -> liftIO $ do -- this scenario might not occur - if the the record is empty it will match the empty list
                  let filemap=FServer ip port  
                  FSA.withMongoDbConnection $ upsert (select ["dirName" =: dirname] "Directory_RECORD") $ toBSON $ FServerMap dirname (Just filemap) []
                  return True
                otherwise -> liftIO $ do 
                  let filemap=  FServer ip port  
                  let newList =fileservers ++ [filemap] 
                  FSA.withMongoDbConnection $ upsert (select ["dirName" =: dirname] "Directory_RECORD") $ toBSON $ FServerMap dirname primary newList 
                  return True 
            (False) -> return False
        [] -> liftIO $ do
          let filemap=FServer  ip port  
          FSA.withMongoDbConnection $ upsert (select ["dirName" =: dirname] "Directory_RECORD") $ toBSON $ FServerMap dirname (Just filemap) []
          return True
    

    
    uploadToShadowDir :: Message4 -> Handler [FInfoTransfer] -- to send empty list if false
    uploadToShadowDir msg@(Message4 encdir encfname enctransId ticket) =  liftIO $ do
       let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
       let (dir ,fname ,transId) =decryptMessage4  msg
       dirEx <- dirExists dir
       case dirEx of
          (True) -> liftIO $ do 
            fileinfo@(FInfo path dirname fileid timestamp)<- newShadowFileInfo dir fname   
            (ip,port) <- getPCAddr dir -- primary copy

            let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
            let toSendback = encryptFInfoTransfer (FInfoTransfer path dirname fileid ip port timestamp) seshkey
              
            result <- appendtoShadowDir fileinfo transId -- append to list of transactions
            let shadowChanges = DirShadowChanges transId result
            FSA.withMongoDbConnection $ upsert (select ["trid2" =: transId] "Shadow_Files_RECORD") $ toBSON $ shadowChanges  -- file mapping
            
            
            return [toSendback]  --- return file reference
              
          (False) -> return $ ([] :: [FInfoTransfer])
    


    commitDirChanges :: Message1-> Handler Bool
    commitDirChanges (Message1 transId) = liftIO $ do
      docs <- FSA.withMongoDbConnection $ find (select ["trid2" =: transId] "Shadow_Files_RECORD") >>= FSA.drainCursor 
      let  docs'= catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DirShadowChanges) docs 
      case docs' of
        [] -> return False
        [modifications] -> do 
          commitShadowChangsToDB  modifications
          FSA.withMongoDbConnection $ delete (select ["trid2" =: transId] "Shadow_Files_RECORD") --delete
          return True

    
    abortDirChanges :: Message1-> Handler Bool
    abortDirChanges (Message1 transId) = liftIO $ do
      FSA.withMongoDbConnection $ delete (select ["trid2" =: transId] "Shadow_Files_RECORD")  
      return True    

    heartbeat  :: Message-> Handler Bool
    heartbeat (Message host port)= liftIO $ do 
      FSA.warnLog $ " Recieved heartbeat from fileserver at port "  ++ port
      let key = host++port
      tm <- getCurrentTime  
      FSA.withMongoDbConnection $ repsert (select ["id1" =: key] "DirHealth_RECORD") $ toBSON $ PingInfo key (show tm)
      -- need  a reccord to store all the info
      return True

    getAllReplicas  :: Maybe String -> Handler[FServer]
    getAllReplicas Nothing = return $ ([] :: [FServer])
    
    getAllReplicas (Just dir)= liftIO $ do
      dirInfo <-getdirs dir
      case dirInfo of
        ((FServerMap _ _ replicas):_) -> return replicas
        ([]) ->  return $ ([] :: [FServer])
-----------------------------------------------helper
-----------------------------------------------------------------------------
--                         Helper functions
-----------------------------------------------------------------------------

dirExists :: String -> IO Bool
dirExists dirName =do  
  FSA.withMongoDbConnection $ do
    docs <- find (select ["dirName" =: dirName] "Directory_RECORD") >>= FSA.drainCursor
    let info =  catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FServerMap) docs
    case info of 
      [] ->  return False 
      otherwise -> return True

-- stores running counter of the ids
getNewFId :: String -> IO String
getNewFId dir = do 
  FSA.withMongoDbConnection $ do
    docs <- find (select ["dName" =: dir] "FileIdCounter_RECORD") >>= FSA.drainCursor
    let counters =   take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FIdCounter) docs
    case counters of
      ((FIdCounter dName val):_) ->   liftIO $ do
        let oldId= read val :: Int 
        let id =oldId+1
        let idStr= show id
        FSA.withMongoDbConnection $ upsert  (select ["dName" =: dir] "FileIdCounter_RECORD") $ toBSON $ FIdCounter dir idStr
        return idStr

      [] ->  liftIO $ do
        FSA.withMongoDbConnection $ upsert  (select ["dName" =: dir] "FileIdCounter_RECORD") $ toBSON $ FIdCounter dir "0"
        return $ "0"
 

-- Gets a new id if this file is not in this directory
getFileId :: String->String -> IO(String)
getFileId dir fname = do
  let filepath=dir++ fname
  exists <- fileExistInDB  filepath
  case exists of
    (False) -> getNewFId dir
    (True)->  liftIO $ do   
      let filepath= dir++fname 
      FSA.withMongoDbConnection $ do
        docs <-  find (select ["filepath" =: filepath] "Files_RECORD") >>= FSA.drainCursor
        let [(FInfo _ _ id _)] =   take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FInfo) docs 
        return id


fileExistInDB :: String -> IO (Bool)
fileExistInDB filepath= liftIO $ do
  FSA.withMongoDbConnection $ do
   
    docs <-  find (select ["filepath" =: filepath] "Files_RECORD") >>= FSA.drainCursor
    let res = take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FInfo) docs 
    case res of 
      [] -> return False
      otherwise -> return True 

  -- there seems to be a lot of database querries, mainly because each file server is treated as a directory        
  -- same file name could be used in another directory      

-- Appending fname to the list of files that this directory has.



genericFileInfo :: String -> String -> IO FInfo  
genericFileInfo  dir fname =  do 
  id <- getFileId dir fname 
  let filepath = dir ++fname 
  tm <- getCurrentTime  
  let fileinfo= FInfo filepath dir id  (show tm)

  return fileinfo   --- return file reference
-- This is for getting a fileinfo for shadow change
--- it should only append a list of files a directory holds, after transaction is commited
newShadowFileInfo  :: String -> String -> IO FInfo  
newShadowFileInfo  dir fname   =  do
  fileinfo <- genericFileInfo dir fname 
  return fileinfo   --- return file reference

createFileInfo :: String -> String-> IO FInfo  --- 
createFileInfo  dir fname  =  do
  fileinfo <- genericFileInfo  dir fname 
  let filepath = dir ++fname
  appendIfNew  dir filepath  
  return fileinfo   --- return file reference
            


commitShadowChangsToDB':: FInfo -> IO ()
commitShadowChangsToDB' fileinfo@(FInfo filepath dir _  _ ) = do
  FSA.withMongoDbConnection $ upsert (select ["filepath" =: filepath] "Files_RECORD") $ toBSON fileinfo
  appendIfNew dir filepath

commitShadowChangsToDB  :: DirShadowChanges -> IO ()
commitShadowChangsToDB  (DirShadowChanges _  docs)= do
  foldM (\ a chg ->commitShadowChangsToDB' chg) () docs
  
  

  return () --- we dont need the result of the map 

-- dirs : replicas
-- pc   : primary copy
-- fileservers associated with the dir name
getdirs :: String -> IO [FServerMap]
getdirs dir= liftIO $ do 
  docs <- FSA.withMongoDbConnection $ find(select ["dirName" =: dir] "Directory_RECORD")>>= FSA.drainCursor
  return $ take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FServerMap) docs

simpleLoadBalance ::[FServer]-> Maybe FServer-> IO (Maybe FServer)
simpleLoadBalance [] pc= return pc
simpleLoadBalance dirs _=  do 
  let len= ((DL.length dirs)-1)
  i <- randomRIO (0, len)

  return $ Just (dirs !! i)
-- randomly picks a directory from a list replicas and sends ip and port
-- reads go to replicas
pickReplica :: String -> IO (String, String)
pickReplica dir=liftIO $ do 
  dirInfo <-getdirs dir
  case dirInfo of 
    ([FServerMap dir pc dirs]) ->do 
      
      serv <-simpleLoadBalance dirs pc
      case (serv ) of 
        (Just(FServer  ip  port )) ->  return $ (ip,port)
        (Nothing) -> return $ ("none","none") 
    
    ([]) -> return $ ("none","none")
-- Writes goes to primary copy get
getPCAddr :: String -> IO (String, String)
getPCAddr dir = liftIO $ do
  primary <-getdirs dir
  case primary of 
    ([FServerMap _ pc _]) ->do 
      case pc of
        (Nothing) -> do 
           
          return $ ("none","none")  ----  elect a new pc --dangerous
        (Just a@(FServer ip port)) -> return $ (ip,port)
    ([]) -> return $ ("none","none")

-------------------------------------------------------------
 ------------- Pings----------------------------
-- Checks if the pinginfo in the database is dated  
isHeartBeatDated:: FServer -> IO (Bool)
isHeartBeatDated (FServer ip port)  = do
  FSA.withMongoDbConnection $ do
    let key= ip++port
    docs <- find (select ["id1" =: key] "DirHealth_RECORD")  >>= FSA.drainCursor
    let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe PingInfo) docs 
    case contents of 
        [(PingInfo _ servTm1)]-> liftIO $ do 
          let tm1= (read servTm1) :: UTCTime
          curTime <- getCurrentTime
          let diff = realToFrac (diffUTCTime curTime tm1  ) :: Double  
          return (diff >heartbeatThresh  ) :: IO (Bool)--- if old ------
        [] -> return True -- it doesnt exists




--removes all the dead replicas and returns a list of servers that are alive  
removeDeadReplicas :: [FServer] ->[FServer]-> IO [FServer]
removeDeadReplicas [] rest = do return rest
removeDeadReplicas (x:xs) rest = do
  status <- isHeartBeatDated x
  case status of
    (True) -> removeDeadReplicas xs rest
    (False )-> removeDeadReplicas xs (rest++[x]) 
-- gets a new primary
getNewPC :: [FServer] -> String-> IO (FServerMap)
getNewPC  [] dir = return $ (FServerMap dir(Nothing) [])
getNewPC replicas dir=liftIO $ do
  let newPC= take 1 replicas -- appoint a new primary copy
  case newPC of
    [a]-> do
      let updatedReplicas = drop 1 replicas  -- remove the newly appointed primary copy from the list of replicas
      return $ FServerMap dir (Just a) updatedReplicas
    []-> do  --this were there are no file servers, drop returns empty list if empty list is passed
      let updatedReplicas = drop 1 replicas
      return $ FServerMap dir (Nothing) updatedReplicas
    
-- elects a new primary copy if needed
electPrIfNeeded ::FServerMap-> IO (FServerMap)
electPrIfNeeded (FServerMap dir (Nothing) replicas ) =getNewPC replicas dir
electPrIfNeeded mg@(FServerMap dir (Just pc) replicas )  = liftIO $ do  
  status <- isHeartBeatDated pc
  case status of
    (True) -> do
      result <- getNewPC replicas dir
      return  result  
    (False) -> return mg
-- clears all the dead nodes and elects a new primary copy if needed
removeDeadNodes :: String -> IO ()
removeDeadNodes dirN  = do
  FSA.warnLog $ " clearing dead nodes ." 
  dirInfo <-getdirs dirN 
  case dirInfo of
    ([ mapping@(FServerMap dir pc dirs)]) -> do
      let updatedDirs= removeDeadReplicas dirs []

      toInsert <- electPrIfNeeded mapping-- just return a FServerMap
      FSA.withMongoDbConnection $ upsert (select ["dirName" =: dir] "Directory_RECORD") $ toBSON toInsert
    otherwise -> return ()-- if this directory doesnt exist

 ----------------------------------Ping stuff-----------------------------------
 
-----------------------------------------------

appendtoShadowDir :: FInfo  ->String -> IO [FInfo]
appendtoShadowDir fileinfo transId= liftIO $ do
  docs <-  FSA.withMongoDbConnection $ find (select ["trid2" =: transId] "Shadow_Files_RECORD") >>= FSA.drainCursor
  let changes =   take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe DirShadowChanges) docs
  case changes of 
    [DirShadowChanges _ files] -> do  
      return $  files++[fileinfo]

    [] -> return [fileinfo]

isNewDir ::String-> String -> Maybe FServer -> [FServer]-> Bool 
isNewDir  ip1 port1 (Nothing) _ = True -- There  must not be a replica if there is no primary server
isNewDir  ip1 port1 (Just (FServer pIp pPort) ) fservers = (foldl (&&) True $ (map (\(FServer ip port) ->  port1/=port) fservers) ) && (port1/=pPort)
-- dirname are names are environment variables in the docker compose

appendIfNew :: String -> String -> IO()
appendIfNew dir fname= do  
  let filepath = dir++fname 
  docs <- FSA.withMongoDbConnection $ do find (select ["dirN" =: dir] "FSFileMap_RECORD") >>= FSA.drainCursor
  let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FSContents) docs 
  case contents of
    [(FSContents _ files)] -> do
        let newList= if filepath`elem` files
                      then files
                      else files++[filepath]

        FSA.withMongoDbConnection $ upsert  (select ["dirN" =: dir] "FSFileMap_RECORD") $ toBSON $ FSContents dir newList
    [] -> FSA.withMongoDbConnection $ upsert  (select ["dirN" =: dir] "FSFileMap_RECORD") $ toBSON $ FSContents dir [filepath]
      
  