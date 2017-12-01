{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module FilesystemAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           FilesystemAPI
import           Datatypes

restAPI :: Proxy API
restAPI = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

loadEnvVars :: Maybe String -> ClientM ResponseData
getREADME :: ClientM ResponseData
storeMessage :: Message -> ClientM Bool
searchMessage :: Maybe String -> ClientM [Message]
performRestCall :: Maybe String -> ClientM ResponseData


upload :: FileContents -> ClientM Bool
download:: Message-> ClientM [FInfo]

lock:: Message3-> ClientM Bool
unlock:: Message3->ClientM Bool
islocked::  Maybe String-> ClientM Bool

listdirs :: Maybe String -> ClientM [String] 
listfscontents :: Message -> ClientM [FSContents]
filesearch ::  Message3 -> ClientM [FInfoTransfer]
updateUploadInfo  ::  Message3 -> ClientM [FInfoTransfer]


getTransId :: Message1 -> ClientM ResponseData
uploadToTransaction :: TransactionContents -> ClientM Bool
commit ::  Message -> ClientM Bool
abort  ::  Message -> ClientM Bool

uploadToShadowDir :: Message4 -> ClientM [FInfoTransfer]


login :: UserInfo -> ClientM [ResponseData]
signup :: UserInfo -> ClientM ResponseData
loadPublicKey :: ClientM [ResponseData]
getFile :: Message -> ClientM [FileContents]



-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(loadEnvVars :<|> getREADME :<|> storeMessage :<|> searchMessage :<|> performRestCall :<|> upload :<|> download :<|> lock :<|> unlock:<|>  islocked  :<|> listdirs  :<|>  listfscontents:<|> filesearch :<|> updateUploadInfo :<|> getTransId  :<|> uploadToTransaction :<|>commit :<|> abort :<|>  uploadToShadowDir :<|> login :<|> signup :<|> loadPublicKey :<|> getFile) = client restAPI


