{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module FilesystemAPIServer where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           FilesystemAPI
import           Datatypes

restFSAPI :: Proxy FSAPI
restFSAPI = Proxy

-- | The function type of the interface here.


 
add_dir :: Message3 -> ClientM Bool  
heartbeat:: Message -> ClientM Bool  
getAllReplicas:: Maybe String -> ClientM [FServer]  
broadcastedUpload :: FileContents -> ClientM Bool
readyToCommit  :: Message -> ClientM Bool  

-- | The following provides the implementations of these types 

(add_dir :<|> heartbeat :<|> getAllReplicas :<|> broadcastedUpload :<|> readyToCommit) = client restFSAPI

-- lock server
restLOCKAPI :: Proxy LOCKAPI
restLOCKAPI = Proxy


lockAvailable :: LockTransfer -> ClientM Bool

(lockAvailable) =  client restLOCKAPI



restTSAPI :: Proxy TSAPI
restTSAPI = Proxy
 
uploadToshadow :: TransactionInfoTransfer -> ClientM Bool   
updateRealDB :: String -> ClientM Bool --- nothing the body, post is used since it is changing state 
commitDirChanges :: Message1 -> ClientM Bool
abortDirChanges:: Message1 -> ClientM Bool

(uploadToshadow :<|> updateRealDB :<|> commitDirChanges :<|> abortDirChanges) = client restTSAPI
