{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module EncryptionAPI where
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource

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


--------------encryption--------------------------
sharedSeed ="hello"
fromPublicKey :: PublicKey ->PubKeyInfo
fromPublicKey msg@(PublicKey key_size n e) =   PubKeyInfo strKey strN strE
  where
      strKey =  show key_size
      strN =  show n
      strE=  show e
toPublicKey :: PubKeyInfo ->PublicKey
toPublicKey msg@(PubKeyInfo strKey strN strE) =    PublicKey key_size n e
    where
        key_size = read strKey :: Int  ----   this
        n = read strN :: Integer
        e= read strE :: Integer


fromPrivateKey :: PrivateKey -> PrivKeyInfo
fromPrivateKey msg@(PrivateKey private_pub private_d private_p private_q private_dP private_dQ private_qinv) =  PrivKeyInfo strPub strPrvD strPrvP strPrvQ strPrvDP strPrvDQ strQINV
  where
      strPub  =  fromPublicKey private_pub
      strPrvD =  show private_d
      strPrvP =  show private_p
      strPrvQ =  show private_q
      strPrvDP=  show private_dP
      strPrvDQ=  show private_dQ
      strQINV =  show private_qinv

toPrivateKey ::  PrivKeyInfo -> PrivateKey
toPrivateKey msg@(PrivKeyInfo  strPub strPrvD strPrvP strPrvQ strPrvDP strPrvDQ strQINV) =  PrivateKey   private_pub private_d private_p private_q private_dP private_dQ private_qinv
  where
      private_pub  =  toPublicKey strPub
      private_d    =  read strPrvD :: Integer
      private_p    =  read strPrvP :: Integer
      private_q    =  read strPrvQ :: Integer
      private_dP   =  read strPrvDP :: Integer
      private_dQ   =  read strPrvDQ :: Integer
      private_qinv =  read strQINV :: Integer

encryptPass :: PublicKey -> String -> IO String
encryptPass key password = liftIO $ do
  myRand <- newGenIO :: IO HashDRBG
  let bpass = C.pack password
  let (res,_) = (encrypt myRand key bpass)
  return $ C.unpack res

seshSeed = "testKey"
-- sharedCrypto functions
-- appends null ('\0') characters until multiple of 16
aesPad :: String -> String
aesPad text
  | ((mod (length text) 16) == 0) = text
  | otherwise = aesPad (text ++ "\0")

-- strips null ('\0') characters from end of string
aesUnpad :: String -> String
aesUnpad text = takeWhile (/= '\0') text

-- seed -> string_to_encrypt -> encrypted_string
myEncryptAES :: String -> String -> String
myEncryptAES seed text = do
  let bseed = (BS.pack $ aesPad seed)
      btext = (BS.pack $ aesPad text)
  let myKey = initKey bseed
  let encryption = encryptECB myKey btext
  BS.unpack encryption

-- seed -> string_to_decrypt -> decrypted_string (unpadded)
myDecryptAES :: String -> String -> String
myDecryptAES seed text = do
  let bseed = (BS.pack $ aesPad seed)
      btext = (BS.pack text)
  let myKey = initKey bseed
  let decryption = decryptECB myKey btext
  aesUnpad $ BS.unpack decryption

 

-- mesg is  a message with 2 strings
encryptMesg :: String -> String -> String -> String-> Message3
encryptMesg str1 str2 seshkey ticket=
    let encstr1 = myEncryptAES (aesPad seshkey) (str1)
        encstr2 = myEncryptAES (aesPad seshkey) (str2)
    in Message3 encstr1 encstr2 ticket

encryptMesg1:: String -> String -> String-> Message
encryptMesg1  str1 seshkey ticket=
    let encstr1 = myEncryptAES (aesPad seshkey) (str1) 
    in Message encstr1 ticket


encryptMesg3:: String -> String ->  String -> String -> String-> Message4
encryptMesg3 str1 str2 str3 seshkey ticket=
    let encstr1 = myEncryptAES (aesPad seshkey) (str1)
        encstr2 = myEncryptAES (aesPad seshkey) (str2)
        encstr3 = myEncryptAES (aesPad seshkey) (str3)
    in Message4 encstr1 encstr2 encstr3 ticket

-- for services
decryptMessage4 ::Message4-> (String,String,String)
decryptMessage4 (Message4 encstr1 encstr2  encstr3 ticket) =
  let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket)
      str1 = myDecryptAES (aesPad seshkey) (encstr1)
      str2 = myDecryptAES (aesPad seshkey) (encstr2)
      str3 = myDecryptAES (aesPad seshkey) (encstr3)
  in (str1, str2,str3)



decryptMessage:: Message -> String -> String
decryptMessage (Message encstr1 _) seshkey=  
  let str1 = myDecryptAES (aesPad seshkey) (encstr1) 
  in str1
decryptMessage3:: Message3  -> (String,String)
decryptMessage3 (Message3 encstr1 encstr2 ticket)  =  
  let seshkey  = myDecryptAES (aesPad sharedSeed) (ticket) 
      str1 = myDecryptAES (aesPad seshkey) (encstr1)
      str2 = myDecryptAES (aesPad seshkey) (encstr2)
  in (str1, str2)
---in directory service
encryptFSContents ::[FSContents] -> [FSContents] -> String  -> [FSContents] 
encryptFSContents [] result _=  result
encryptFSContents ((FSContents dir files):xs) result seshkey= 
  let encdir = myEncryptAES (aesPad seshkey) (dir)
      encfiles = map (myEncryptAES (aesPad seshkey)) files 
  in encryptFSContents xs ([FSContents encdir encfiles] ++result) seshkey 
--- this is for client 
decryptFSContents ::[FSContents] -> [FSContents] -> String -> [FSContents] 
decryptFSContents [] result _=  result
decryptFSContents ((FSContents dir files):xs) result seshkey= 
  let decdir = myDecryptAES (aesPad seshkey) (dir)
      decfiles = map (myDecryptAES (aesPad seshkey)) files 
  in decryptFSContents xs ([(FSContents decdir decfiles)] ++result) seshkey


encryptFInfoTransfer :: FInfoTransfer -> String   -> FInfoTransfer
encryptFInfoTransfer (FInfoTransfer path dir fileid ip port timestamp  ) seshkey    =
  let encpath = myEncryptAES (aesPad seshkey) (path)
      encdir = myEncryptAES (aesPad seshkey) (dir)
      encfileid = myEncryptAES (aesPad seshkey) (fileid)
      encip = myEncryptAES (aesPad seshkey) (ip)
      encport = myEncryptAES (aesPad seshkey) (port)
      enctimestamp = myEncryptAES (aesPad seshkey) (timestamp)

  in  FInfoTransfer encpath encdir encfileid encip encport enctimestamp 

decryptFInfoTransfer :: String ->FInfoTransfer ->  FInfoTransfer
decryptFInfoTransfer seshkey (FInfoTransfer path dir fileid ip port timestamp  ) =
  let decpath = myDecryptAES (aesPad seshkey) (path)
      decdir = myDecryptAES (aesPad seshkey) (dir)
      decfileid = myDecryptAES (aesPad seshkey) (fileid)
      decip = myDecryptAES (aesPad seshkey) (ip)
      decport = myDecryptAES (aesPad seshkey) (port)
      dectimestamp = myDecryptAES (aesPad seshkey) (timestamp)

  in  FInfoTransfer decpath decdir decfileid decip decport dectimestamp 


encryptFileContents :: FileContents -> String -> String -> FileContents
encryptFileContents (FileContents path contents _) seshkey ticket =
  let encpath = myEncryptAES (aesPad seshkey) (path)
      encContents = myEncryptAES (aesPad seshkey) (contents)
  in  FileContents encpath encContents ticket
decryptFileContents :: FileContents -> String -> FileContents
decryptFileContents (FileContents path contents _) seshkey=
  let decpath = myDecryptAES (aesPad seshkey) (path)
      decContents = myDecryptAES (aesPad seshkey) (contents) 
  in  FileContents decpath decContents  ""


encryptTransactionContents :: TransactionContents -> String -> String-> TransactionContents
encryptTransactionContents (TransactionContents trId (TChanges fileinfotransfer filecontents ) _) seshkey ticket =
  let enctrId = myEncryptAES (aesPad seshkey) (trId)
      encfileContents = encryptFileContents  filecontents seshkey ticket
      encfileInfoTran=  encryptFInfoTransfer fileinfotransfer seshkey 
  in  TransactionContents enctrId  (TChanges encfileInfoTran encfileContents ) ticket

decryptTransactionContents :: String -> TransactionContents ->  TransactionContents
decryptTransactionContents seshkey (TransactionContents trId (TChanges fileinfotransfer filecontents ) _) =
  let dectrId = myDecryptAES (aesPad seshkey) (trId)
      decfileContents = decryptFileContents  (filecontents) seshkey
      decfileInfoTran= decryptFInfoTransfer seshkey fileinfotransfer 
  in  TransactionContents dectrId  (TChanges decfileInfoTran decfileContents ) ""


encryptList :: [String] -> String -> [String]-> [String]

encryptList [] _ result =result
encryptList (x:xs) seshkey result =
  let encrp= myEncryptAES (aesPad seshkey) (x) 
  in  encryptList xs seshkey (result++[encrp])
