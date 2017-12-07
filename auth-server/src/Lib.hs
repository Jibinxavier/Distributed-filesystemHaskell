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
import           Data.Time.Clock              (UTCTime, getCurrentTime, addUTCTime)
import           Data.Time.Clock.POSIX
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
import qualified FilesystemAPI as FSA  
import           Datatypes 
import           EncryptionAPI

 

type API1 =  "login"                      :> ReqBody '[JSON] UserInfo  :> Post '[JSON] [ResponseData]
      :<|> "signup"                     :> ReqBody '[JSON] UserInfo  :> Post '[JSON]  ResponseData --storemessage 
      :<|> "loadPublicKey"              :> Get '[JSON] [ResponseData]


startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = FSA.withLogging $ \ aplogger -> do

  FSA.warnLog "Starting filesystem"
  port <- FSA.authPortStr
  let settings = setPort ((read $ port):: Int) $ setLogger aplogger defaultSettings
  runSettings settings app


app :: Application
app = serve api server

api :: Proxy API1
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API1
server = login
    :<|> signup 
    :<|> loadPublicKey


  where
    
    decryptPass :: String ->  IO String
    decryptPass password =   do
        let auth=  "auth" :: String
        FSA.withMongoDbConnection $ do
          keypair <- find (select ["owner" =: auth] "Keys") >>= FSA.drainCursor
          let [(Keys _ pub prv)]= catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Keys) keypair
          let prvKey= toPrivateKey prv
          let pass=  C.pack password
          let decPass=decrypt prvKey pass
          return $ C.unpack decPass
  
    toResponseData :: PubKeyInfo -> [ResponseData]
    toResponseData msg@(PubKeyInfo strKey strN strE)=((ResponseData $ strKey):(ResponseData $ strN):(ResponseData $ strE):[])

    loadPublicKey :: Handler [ResponseData]
    loadPublicKey= liftIO $ do
      FSA.withMongoDbConnection $ do
        let auth=  "auth" :: String

        docs <- find (select ["owner" =: auth] "Keys") >>= FSA.drainCursor

        let pubKey= catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Keys) docs
        case pubKey of
          [(Keys _ pub prv)]-> return $ toResponseData pub
          [] -> liftIO $ do
            r1 <- newGenIO :: IO HashDRBG
            let (pub,priv,g2) = generateKeyPair r1 1024
            let strPubKey = fromPublicKey pub
            let strPrvKey = fromPrivateKey priv
            let key = Keys auth strPubKey strPrvKey
            FSA.withMongoDbConnection $ upsert (select  ["owner" =: auth] "Keys") $ toBSON key
            return $ toResponseData strPubKey


    



    signup :: UserInfo -> Handler ResponseData
    signup msg@(UserInfo key password) = liftIO $ do
      decPassStr <- decryptPass password
      FSA.warnLog $ "Storing UserInfo under key " ++ key ++ "."
      FSA.withMongoDbConnection $ do
        docs <- findOne (select ["username" =: key] "Users")


        case docs of
          (Just _) -> return $ ResponseData $  "Already signed up "
          (Nothing) -> liftIO $ do
            hash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack decPassStr)
            let passChars= BS.unpack $ fromJust hash      -- converted from bytestring to [char]( string)
            FSA.withMongoDbConnection $ upsert (select ["username" =: key] "Users") $ toBSON msg {password = passChars}
            return $ ResponseData $  "Signed up "



    
    isValid :: [UserInfo] -> String -> Bool
    isValid ((UserInfo _ hash):_) password = validatePassword (BS.pack hash) (BS.pack password)
    isValid _ _=False

    login :: UserInfo -> Handler [ResponseData]
    login msg@(UserInfo key password) =  liftIO $ do
      FSA.warnLog $ "Searching for user  for key: " ++ key
      decPassStr <- decryptPass password
      FSA.withMongoDbConnection $ do
        docs <- find (select ["username" =: key] "Users") >>= FSA.drainCursor
        let userInfo = take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe UserInfo) docs
        let validity=  isValid userInfo decPassStr


        case  validity of
          True ->  liftIO $   do
            expirydate <- getExpirydate
            let ss = (aesPad sharedSeed)
            let ticket= myEncryptAES ss seshSeed

            let encExpirydate = myEncryptAES (aesPad decPassStr) (expirydate)  
            let encSesh= myEncryptAES (aesPad decPassStr) (seshSeed)  
           
            return $ ((ResponseData $ ticket):(ResponseData $ encSesh):(ResponseData   encExpirydate):[]) -- RETURN 'TOKEN' WITH HASHED CONTENTS
          False  ->return $ ((ResponseData $ "Hmm.. Sketchy password "):[])


  
getExpirydate :: IO String
getExpirydate = do
  let daylength = posixDayLength
  curtime <- getCurrentTime
  let lifespan= show $ addUTCTime daylength curtime
  return lifespan 