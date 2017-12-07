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
module Helpers where
import           System.IO
import           Control.Monad                 
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Bson.Generic 
import           Distribution.PackageDescription.TH
import           Git.Embed
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Options.Applicative
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           System.Console.ANSI
import           System.Environment
import           FilesystemAPI
import           FilesystemAPIClient 
import           Data.Time.Clock
import qualified Data.List                    as DL
import           Database.MongoDB       
import           Data.Maybe
import           GHC.Generics
import           Data.Text                    (pack, unpack)
import           Datatypes 
import           EncryptionAPI
-- | to embed git and cabal details for this build into the executable (just for the fun of it)
-- The code inside $( ) gets run at compile time. The functions run extract data from project files, both .git files and
-- the .cabal file.
mydoCall2 act f port= reportExceptionOr (act) (SC.runClientM f =<< envFileApi port) 
-- | a simple handler class to print the response. We can pass this as the first parameter of calls to
-- reportOrException. It will call the appropriate version depending on the type of the returned results.
doCall f h p seshkey= reportExceptionOr (putStrLn . resp seshkey) (SC.runClientM f =<< env h p)


--- to format File server information

formatFServerMap (FServerMap dirname (Just primary) [])= " Dir name: "++dirname++"Primary Ip " ++ (ip primary)  ++" port "++(ip primary)++ "No replicas"
formatFServerMap (FServerMap dirname (Nothing) [])= " Dir name: "++dirname++" No primary or replica"
formatFServerMap (FServerMap dirname (Just (FServer ip port)) fservers)= 
  let primary =" Dir name: "++dirname++" Primary Ip " ++ (ip)  ++" port "++(port  )++ "\n"
      replicas= "Replicas "++ ((DL.intercalate ", " $ map (\(FServer ip port) -> " port " ++port ++ "  ip  "++ip) fservers))
  in primary++ replicas


class PrintResponse a where
  resp :: Show a =>String ->   a  -> String

instance PrintResponse ResponseData where
  resp  seskey r  = "Response is a single value: " ++(response r)

instance PrintResponse [Message] where
  resp  _ []  = "No messages."
  resp  _ [x]   = "Response is a single message: " ++ message x 
  resp   _ rs  = "Response is an array with messages: " ++ (DL.intercalate ", " $ map message rs)
instance PrintResponse String where
  resp  seskey x= "Response is a single message: " ++(myDecryptAES (aesPad seskey) x)
instance PrintResponse [String] where
  resp   _ []= "No messages."
  resp seskey [x] = "Response is a single string: " ++ (myDecryptAES (aesPad seskey) x)
  
  resp  seskey rs  = "Response is an array with messages: " ++ (DL.intercalate ", " $ map (myDecryptAES (aesPad seskey)) rs )

instance PrintResponse [ResponseData] where-- no ecrypted message back
  resp  _ rs   = "Response is an array with values: " ++ (DL.intercalate ", " $ map response rs)
instance PrintResponse [FileContents] where-- no ecrypted message back
  resp _ rs  = "Response is an array with values: " ++ (DL.intercalate ", " $ map contents rs)

instance PrintResponse Bool where
  resp _ True =  "Response is a boolean : Totally!"
  resp _ False  = "Response is a boolean : Like No Way!"
instance PrintResponse [FSContents] where
  resp _ [] = " File server is empty"
  resp seskey rs = 
    let [(FSContents dirN files)] =decryptFSContents rs [] seskey
    in "Directory server files . Dir name:    " ++ dirN ++(DL.intercalate "   ," files)
  resp _ rs ="Response is from directory server: need to add  this case "


  
instance PrintResponse [FInfoTransfer] where 
  resp _ []  = " No files"
  resp seskey [rs]  = 
    let (FInfoTransfer filepath dirname fileid ipadr  portadr timestamp ) =decryptFInfoTransfer  seskey rs
    in "File info. Filepath: " ++ filepath ++ " dirname "++dirname++ " fileid "++fileid++" portadr "++portadr++ " timestamp "++timestamp
-- | Command line option handlers, one for each command
-- These are called from the options parsing and do the actuall work of the program.

-- let's put all the hard work in a helper...


-- which makes the actual rest calls trivial...(notice the currying)
-------------------------------------------------------------------
--------   Helpers  ------------------
getFileFromFS ::  [FInfoTransfer]-> String -> IO ()
getFileFromFS fileinfo@[FInfoTransfer filepath dirname fileid _ portadr servTm1 ] usern= do 

  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> do 
      let encFid= myEncryptAES (aesPad seshkey) (fileid) 

      res <- mydoCall (getFile $ Message encFid ticket ) ((read portadr):: Int)
      
      case res of
        Left err -> do
          putStrLn $ "get file call to fileserver  failed with error: " ++ show err
        Right (a) -> do 
          case a of 
            [resp] -> do
              let mg@(FileContents fName contents _) =  decryptFileContents resp seshkey
              -- updating the file info in the local record
              withMongoDbConnection $ upsert (select ["filepath" =: filepath] "CLIENTFileMap_RECORD") $ toBSON $ FInfo filepath dirname fileid servTm1
              writeFile fName contents 
            [] -> putStrLn "Incorrect filepath and filename  "
    (Nothing) -> putStrLn $ "Expired token . Sigin in again.  " 

  
    
getLocalTrId ::IO ([LocalTransInfo])
getLocalTrId = do
  let key = "client1":: String --- maybe an environment variable in the docker compose
  docs <- withMongoDbConnection $ find  (select ["key1" =: key] "Transaction_RECORD")  >>= drainCursor -- getting previous transaction id of the client
  return $ take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe LocalTransInfo) docs 
clearTransaction :: IO ()
clearTransaction = do
  let key = "client1":: String --- maybe an environment variable in the docker compose
  withMongoDbConnection $ delete  (select ["key1" =: key] "Transaction_RECORD")
gitRev, gitBranch, cabalAuthor, cabalVersion, cabalCopyright :: String
gitRev = $(embedGitShortRevision)
gitBranch = $(embedGitBranch)
cabalAuthor = $( packageVariable  (packageString . author))
cabalVersion = $( packageVariable (pkgVersion . package))
cabalCopyright = $( packageVariable (packageString . copyright ))

-- | helper functions to change color in ansi terminal output (mor for the fun of it)
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]




seshNop = "nop to avoid errors" :: String

-- | output a command line banner
banner :: IO ()
banner = do
  progName <- getProgName
  putStrLn $ "\n" ++ redCode ++ progName ++ " (" ++ cabalVersion ++ ")  - " ++
             cabalCopyright ++" (" ++ cabalAuthor ++ ")" ++ resetCode ++ "\n" ++
             whiteCode ++ "Git Branch: " ++ gitBranch ++ ", Git Revision: " ++ gitRev ++ resetCode ++ "\n"

-- | A helper function to make it easier to execute rest calls and report errors if the occur
reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''


 -- stores the session key
storeClientAuthInfo cname pass ((ResponseData ticket):(ResponseData seskey):(ResponseData expiryDate):rest) = do
  let decSesh = myDecryptAES (aesPad pass) (seskey)
      decexpiryDate = myDecryptAES (aesPad pass) (expiryDate)
  putStrLn "CLIENT: Logged in successfully"
  (withMongoDbConnection $ upsert (select ["cname" =: cname] "ClientInfo_RECORD") $ toBSON $ ClientInfo cname decSesh ticket decexpiryDate)
   
  

storeClientAuthInfo _ _ [ResponseData errmsg] = putStrLn $ " Auth error . " ++  errmsg




-- helpers to simplify the creation of command line options
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                     <> short 'i'
                                     <> metavar "IPADDRESS"
                                     <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")



-- | function to build the client environment for performing a servant client rest call
-- It uses the host name and port parameters if Just x, or else uses envrionment variables
-- This uses an applicative programming style that is very condensed, and easy to understand when you get used to it,
-- compared to the alternative sequence of calls and subsequent record construction.

env :: Maybe String -> Maybe String -> IO SC.ClientEnv
env host port = SC.ClientEnv <$> newManager defaultManagerSettings
                                               <*> (SC.BaseUrl <$> pure SC.Http
                                                               <*> (host <?> usehaskellHost)
                                                               <*> (read <$> (port <?> usehaskellPort))
                                                               <*> pure "")
 where
   (<?>) :: Maybe a -> IO a -> IO a
   h <?> f = case h of
     Just hst -> return hst
     Nothing  -> f

   -- | The url endpoint for contactingt the use-haskell service
   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True

   -- | The neo4j port
   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8079" True


   -- | Helper function to simplify the setting of environment variables
   -- function that looks up environment variable and returns the result of running funtion fn over it
   -- or if the environment variable does not exist, returns the value def. The function will optionally log a
   -- warning based on Boolean tag

   -- note that this is a good example of a commonly required function that could usefully be put in a shared library
   -- but I'm not going to do that for now.

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fn def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fn s
         Nothing -> warn' warn env def

    where warn' :: Show b => Bool -> String -> b -> IO b
          warn' wn e s =  do
            when wn $ putStrLn $ "Environment variable: " ++ e ++
                                    " is not set. Defaulting to " ++ (show s)
            return s
-- helpers 
isDated :: String ->String -> IO Bool
isDated filepath servTm1 = do
  withMongoDbConnection $ do
          docs <- find (select ["filepath" =: filepath] "CLIENTFileMap_RECORD") >>= drainCursor
          let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FInfo) docs 
          case contents of 
              [(FInfo _ _ _  strTm2)]-> liftIO $ do 
                 let tm1= (read servTm1) :: UTCTime
                 let tm2= (read strTm2) :: UTCTime
                 let diff = realToFrac (diffUTCTime tm1 tm2 ) :: Double  
                 return (diff >0  ) :: IO (Bool)--- if old

              [] -> return True -- return true if it is not found locally


updateLocalMeta :: String ->FInfo -> IO ()
updateLocalMeta filepath fileinfo = do
  withMongoDbConnection $ upsert (select ["filepath" =: filepath] "CLIENTFileMap_RECORD") $ toBSON $ fileinfo
  return ()              


isFileLocked :: String -> IO Bool
isFileLocked filepath  = liftIO $ do
  port <- lockPortStr
  res <- mydoCall (islocked $ Just filepath) ((read port):: Int)
  case res of
    Left err -> do
      putStrLn $ " call to lock server  failed with error: " ++ show err
      return True
    Right (state) -> do 
      return state
appendToLockedFiles :: String  -> String-> IO()
appendToLockedFiles filepath tid = liftIO $ do
   docs <- withMongoDbConnection $ find (select ["tid5" =: tid] "LockedFiles_RECORD") >>= drainCursor
   let  contents= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe LockedFiles) docs 
   case contents of 
    [] ->  do -- new transaction
      withMongoDbConnection $ upsert (select ["tid5" =: tid] "LockedFiles_RECORD") $ toBSON $ LockedFiles tid [filepath]
      return ()
    [LockedFiles _ files] -> do -- adding to existing transaction
      let updatedList=files ++ [filepath]
      withMongoDbConnection $ upsert (select ["tid5" =: tid] "LockedFiles_RECORD") $ toBSON $ LockedFiles tid updatedList


isvalidSession :: String -> IO Bool
isvalidSession expiryDate = do
  curtime <- getCurrentTime
  let expiryDate'= (read expiryDate) :: UTCTime
  let diff = realToFrac (diffUTCTime expiryDate' curtime ) :: Double  
  return (diff >0.0  ) :: IO (Bool)  

getAuthClientInfo :: String  -> IO(Maybe (String , String) )   
getAuthClientInfo cname =  do
   docs <- withMongoDbConnection $ find (select ["cname" =: cname] "ClientInfo_RECORD")   >>= drainCursor -- getting previous transaction id of the client
  
   let  clientInfo= take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ClientInfo) docs 
  
   case clientInfo of 
    [] -> return Nothing
    [ClientInfo cname seskey ticket expiryDate] -> do
      status <- isvalidSession expiryDate
      case status of
        True -> return $ Just (ticket,  seskey)
        False -> return $ Nothing
docallMsg3WithEnc restCall str1 str2 usern ip port= do
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> do 
      let msg =encryptMesg str1 str2 seshkey ticket
      doCall  (restCall $ msg) ip port seshkey
    (Nothing) -> putStrLn $ "Expired token . Sigin in again.  " 

docallMsg1WithEnc restCall str1 usern ip port= do
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> do 
      let msg =encryptMesg1 str1  seshkey ticket
      doCall  (restCall $ msg) ip port seshkey
    (Nothing) -> putStrLn $ "Expired token . Sigin in again.  " 

mydoCalMsg3WithEnc restCall str1 str2 usern port decryptFunc= do
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> liftIO $ do 
      let msg =encryptMesg str1 str2 seshkey ticket
      res <- mydoCall  (restCall $ msg)  port 
      case res of
        Left err -> do
          putStrLn $ " call failed with error: " ++ show err
          return Nothing
        Right (resp) ->  return $ Just $ map (decryptFunc seshkey)  resp
    (Nothing) -> do 
      putStrLn $ "Expired token . Sigin in again.  "
      return Nothing


mydoCalMsg4WithEnc restCall str1 str2  str3 usern port decryptFunc= do
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> liftIO $ do 
    
      let msg = encryptMesg3 str1 str2 str3 seshkey ticket
      res <- mydoCall  (restCall $ msg)  port 
      case res of
        Left err -> do
          putStrLn $ " call failed with error: " ++ show err
          return Nothing
        Right (a) ->  return $ Just  $ map (decryptFunc seshkey) a 
    (Nothing) -> do 
      putStrLn $ "Expired token . Sigin in again.  "
      return Nothing
mydoCalMsg1WithEnc restCall   usern port= do
  authInfo <- getAuthClientInfo usern
  case authInfo of 
    (Just (ticket,seshkey) ) -> liftIO $ do 
     
      res <- mydoCall  (restCall $ Message1 ticket)  port 
      case res of
        Left err -> do
          putStrLn $ " call failed with error: " ++ show err
          return Nothing
        Right (a) ->  return $ Just a
    (Nothing) -> do 
      putStrLn $ "Expired token . Sigin in again.  "
      return Nothing
