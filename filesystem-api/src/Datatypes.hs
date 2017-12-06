{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Datatypes where
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
data LockTransfer = LockTransfer { filepathL :: String -- used for transfering the lock to the client
                  , lstatus :: Bool 
                 }deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data Lock = Lock { filename :: String -- for storing lock in lock database
                  , status :: Bool
                  , usern:: String
                  , queue:: [[String]] -- (usern, addr)
                 }deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)
deriving instance FromBSON [[String]]
deriving instance ToBSON [[String]]
data Message1 = Message1 { file    :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)
data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)
data Message3 = Message3 { str1    :: String
                        , str2 :: String
                        , str3 :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)
data Message4 = Message4 { str11    :: String
                        , str12 :: String
                        , str13 :: String
                        , str14 :: String 
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)
data FileContents = FileContents { fileP    :: String
                          ,contents :: String
                          ,ticket1 :: String
                      } deriving (Show, Eq,Generic, FromJSON, ToJSON, ToBSON, FromBSON)
data UserInfo = UserInfo { username    :: String
                      , password :: String
                      } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)
data PubKeyInfo = PubKeyInfo { key_size :: String
                        , key_n :: String
                        , key_e :: String
                       }deriving (Show, Eq, Generic, FromJSON, ToBSON, FromBSON, ToJSON)
data PrivKeyInfo = PrivKeyInfo {
                          prv_pub :: PubKeyInfo
                          , prv_d :: String
                          , prv_p :: String
                          , prv_q :: String
                          , prv_dP :: String
                          , prv_dQ :: String
                          , prv_qinv :: String
                        } deriving (Show, Eq,Generic, FromJSON, ToBSON, FromBSON, ToJSON)
data Keys= Keys {
                  owner   :: String
                  ,pubickey   :: PubKeyInfo
                  ,privateKey:: PrivKeyInfo
            }deriving (Show, Eq,Generic, FromJSON, ToBSON, FromBSON, ToJSON)

data ClientInfo = ClientInfo {
              cname :: String
            , seskey :: String
            , ticket2:: String
            , expiryDate :: String 
            }deriving (Show, Eq,Generic, FromJSON, ToBSON, FromBSON, ToJSON)
deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

 

data FInfo = FInfo {
                           filepath     :: String -- directory (which is a fileserver)++ filename
                        ,  dirname      :: String
                        ,  fileid       :: String 
                        ,  timestamp    :: String
                       } deriving (Generic, FromJSON, ToBSON,ToJSON, FromBSON,Show,Eq)
data FInfoTransfer = FInfoTransfer {
                        filepath2     :: String -- directory (which is a fileserver)++ filename
                      ,  dirname2      :: String
                      ,  fileid2       :: String
                      ,  ipadr        :: String    --- to reduce the number querries
                      ,  portadr      :: String
                      ,  timestamp1    :: String 
                      } deriving (Generic, FromJSON, ToBSON,ToJSON, FromBSON,Show,Eq)
 
 
data FSContents = FSContents { -- dir mapping
                           dirN   :: String
                        ,  files     :: [String]  
                        
                       } deriving (Show,Generic,Eq, FromJSON, ToBSON,ToJSON, FromBSON)

                       


data FServer= FServer{
                        ip       :: String
                      ,  port    :: String
                     }deriving (Show,Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON)


data FServerMap= FServerMap{
                        dirName  :: String
                      , primary :: Maybe FServer
                      ,  fservers    :: [FServer]
                
                     }deriving (Show,Generic, FromJSON,ToJSON, ToBSON, FromBSON)
deriving instance FromBSON [FServer]
deriving instance ToBSON [FServer] 
deriving instance ToBSON Bool
deriving instance FromBSON Bool 
data FIdCounter= FIdCounter{ ------ file id counter for each directory
                        dName   :: String
                      , curId       :: String 
                     }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON)     

data FSrvIdCounter= FSrvIdCounter{ ------ fileserver id counter  
                       cId       :: String 
                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON)  

data TrCounter= TrCounter{ ------ transation id counter  
                       id22      :: String
                       ,tid       :: String
                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON)   


deriving instance FromBSON [String]  -- we need these as BSON does not provide
deriving instance ToBSON   [String]
 
data TChanges= TChanges{ 
                        fileInfotrans   :: FInfoTransfer,
                        fileContents:: FileContents

                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show)   

  

--- contents of the transaction are sent to transaction server
data TransactionContents =TransactionContents {
                         trid :: String     
                      , modifcation :: TChanges
                      ,ticket3 :: String

                   }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 
-- files part of transaction are logged 
data Transaction = Transaction {
                       transid                :: String
                       ,mods                  :: [TChanges]
                       ,toCommit              :: [String] -- 
                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 

data TransactionInfoTransfer = TransactionInfoTransfer { --  information to the shadow space
                        trid1 :: String 
                       ,dir2 :: String
                       ,fileContents1:: FileContents
                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 


data DirShadowChanges = DirShadowChanges { --  information to the transaction server
                        trid2 :: String 
                       ,shadowChanges:: [FInfo]
                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 
data FileServShadowChanges = FileServShadowChanges { --  information to the transaction server
                        trid3 :: String 
                       ,shadowCh:: [FileContents]
                      }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 
                      

data PingInfo = PingInfo { -- id would the port and ip concatenated together
                         id1  :: String
                        ,timestamp3 :: String
                    }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 

data LockedFiles = LockedFiles {  
                         tid5  :: String
                        ,lockedfiles :: [String]
                    }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 
data LocalTransInfo = LocalTransInfo {  
                         key1 :: String
                         ,tid6  :: String 
                    }deriving (Generic,Eq, FromJSON,ToJSON, ToBSON, FromBSON, Show) 
                    
deriving instance FromBSON [TChanges]
deriving instance ToBSON [TChanges] 

-- deriving instance FromBSON [(String,String)]
-- deriving instance ToBSON [(String,String)] 
deriving instance FromBSON [FInfo]
deriving instance ToBSON [FInfo] 
deriving instance FromBSON [FileContents]
deriving instance ToBSON [FileContents] 
deriving instance ToBSON (Maybe FServer)
deriving instance FromBSON (Maybe FServer)
