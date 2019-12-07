module Server where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Exception.Base
import Control.Monad
import Control.Monad.Fix
import GHC.Conc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import  Data.Text.Encoding
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable
import Network
import System.IO
import Text.Printf

import Types
import Parser

data Client = Client
  { clientNick   :: Nick
  , clientHandle :: Handle
  , clientSendCh :: TChan Message
  , clientRooms  :: TVar (S.Set Room) }
 deriving (Eq)

instance Show Client where
  show Client{..} = show clientNick

data Server = Server
  { serverClients :: TVar (M.Map Nick Client)
  , serverRooms :: TVar (M.Map Room (S.Set Nick))
  , serverPrintCh :: TChan String }

port :: Int
port = 23456

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting server..."
  server@Server{..} <- atomically $ newServer

  socket <- listenOn (PortNumber (fromIntegral port))

  forkIO $ forever $ do
    (handle, host, port) <- accept socket
    printf "Accepted connection from %s.\n" host
    forkFinally (talk handle server)
      (\r -> case r of
          (Left e) -> do
            hClose handle
          (Right e) -> do
            hClose handle)

  fix $ \loop -> do
    line <- getLine
    if line == "Quit"
      then do
        atomically $ globalMessage server $ FromServer "Server shutting down."
        return ()
      else loop

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  B.hPut handle
    "Welcome! Please use the :NICK command to choose a nick name.\n"
  getNick

 where
  getNick = mask $ \restore -> do
    cmd <- parseCommand <$> B.hGetLine handle
    case cmd of
      Just (TakeNick nick) -> do
        mbClient <- atomically $ findClient server nick
        case mbClient of
          Just _ -> do
            B.hPut handle "Name already in use.\n"
            getNick
          Nothing -> do
            newClient <- atomically $ do
              client <- newClient handle nick
              modifyTVar serverClients (M.insert nick client)
              sendMessage client $ FromServer $ T.pack $ "Welcome " ++ (show nick) ++ "!"
              return client
            catch (restore (handleClient server newClient)) (\(SomeException e) -> putStrLn "Client quit")
      _ -> do
        B.hPut handle "Please use the :NICK command to choose a nick name.\n"
        getNick


handleClient :: Server -> Client -> IO ()
handleClient server@Server{..} client@Client{..} = do
  race (handleInput) (handleOutput)
  return ()

 where
  handleInput :: IO ()
  handleInput = do
    mbCmd <- catch (parseCommand <$> B.hGetLine clientHandle)
                   (\(SomeException e) -> return $ Just Quit)
    case mbCmd of
      Just cmd ->
        case cmd of
          Quit -> do
            atomically $ quit server client
            putStrLn $ (show clientNick) ++ " has left server.\n"
            return ()
          TakeNick _ -> handleInput -- Changing names not implemented
          Join room -> do
            print $ (show clientNick) ++ " joining " ++ (show room)
            atomically $ joinRoom server client room
            handleInput
          Part room -> do
            print $ (show clientNick) ++ " leaving " ++ (show room)
            atomically $ partRoom server client room
            handleInput
          Names room -> do
            atomically $ names server client room
            handleInput
          List -> do
            atomically $ list server client
            handleInput
          PrivMsg tgts msg -> do
            atomically $ privMsg server client tgts msg
            handleInput
      Nothing ->
        do
        atomically $ sendMessage client $ FromServer "Invalid command."
        handleInput

  handleOutput = forever $ do
      msg <- atomically $ readTChan clientSendCh
      B.hPut clientHandle (encodeUtf8 $ T.pack $ show msg)

-- Initialization

newServer :: STM (Server)
newServer = do
  clients <- newTVar M.empty
  rooms <- newTVar M.empty
  printCh <- newTChan
  return (Server clients rooms printCh)

newClient :: Handle -> Nick -> STM (Client)
newClient handle nick = do
  chan <- newTChan
  rooms <- newTVar S.empty
  return $ Client nick handle chan rooms

-- Command executions

quit :: Server -> Client -> STM ()
quit server@Server{..} client@Client{..} = do
  rooms <- readTVar clientRooms
  traverse_ (partRoom server client) rooms
  modifyTVar serverClients (M.delete clientNick)


joinRoom :: Server -> Client -> Room -> STM ()
joinRoom server@Server{..} client@Client{..} room = do
  cRooms <- readTVar clientRooms
  case S.member room cRooms of
    True -> return () -- Client already in room; do nothing.
    False -> do
      writeTVar clientRooms $ S.insert room cRooms
      roomMap <- readTVar serverRooms
      case M.lookup room roomMap of
        Nothing -> addNewRoom roomMap
        Just _ -> joinExtantRoom roomMap

  where
    addNewRoom roomMap = do
      writeTVar serverRooms $ M.insert room (S.singleton clientNick) roomMap
      messageRoom server room $
        FromServer $ T.pack $ "Created new channel " ++ show room
      return ()

    joinExtantRoom roomMap = do
      writeTVar serverRooms $ M.adjust (S.insert clientNick) room roomMap
      messageRoom server room $
        FromServer $ T.pack $
          show client ++ " has joined " ++ show room ++ "."
      return ()


partRoom :: Server -> Client -> Room -> STM ()
partRoom server@Server{..} client@Client{..} room = do
  cRooms <- readTVar clientRooms
  case S.member room cRooms of
    True -> do
      writeTVar clientRooms $ S.delete room cRooms
      roomMap <- readTVar serverRooms
      writeTVar serverRooms $
        M.update
        (\nicks -> mbEmptySet (S.delete clientNick nicks))
        room roomMap
      messageRoom server room
        $ FromServer $ T.pack $ show client ++ " has left "  ++ show room ++ "."
      return ()
    False -> return () -- Client is not a member of room; do nothing.

  where
    mbEmptySet s = if S.null s then Nothing else Just s

names :: Server -> Client -> Room -> STM ()
names server@Server{..} client room = do
  inRoom <- isClientInRoom server client room
  if inRoom
    then do
      nicks <- (flip (M.!)) room <$> readTVar serverRooms
      forM_ nicks (\nick -> sendMessage client $ FromServer $ T.pack $ show nick)
    else
      sendMessage client $
        FromServer $ T.pack $ "You are not in " ++ (show room) ++ "."

list :: Server -> Client -> STM ()
list Server{..} client = do
  rooms <- M.keys <$> readTVar serverRooms
  forM_ rooms (\room -> sendMessage client $ FromServer $ T.pack $ show room)

privMsg :: Server -> Client -> [Target] -> T.Text -> STM ()
privMsg server@Server{..} client@Client{..} tgts msg = forM_ tgts msgTarget

  where
   msgTarget (TgtRoom room) = do
     inRoom <- isClientInRoom server client room
     if inRoom
     then do
       messageRoom server room $
         FromRoom room clientNick msg
       return ()
     else do
       sendMessage client $
         FromServer $ T.pack $"You are not in " ++ show room ++ "."
       return ()

   msgTarget (TgtNick nick) = do
     mbTgt <- findClient server nick
     case mbTgt of
       Just tgt -> sendMessage tgt $ FromClient clientNick msg
       Nothing  ->
         sendMessage client $
           FromServer $ T.pack $ (show nick) ++ "does not exist on server!"

-- Utilities

findClient :: Server -> Nick -> STM (Maybe Client)
findClient Server{..} nick =
  M.lookup nick <$> readTVar serverClients

isClientInRoom :: Server -> Client -> Room -> STM Bool
isClientInRoom Server{..} Client{..} room = do
  inClientRooms <- S.member room <$> readTVar clientRooms
  mbNicks <- M.lookup room <$> readTVar serverRooms
  case mbNicks of
    Just nicks ->
      if S.member clientNick nicks
      then checkState inClientRooms True
      else checkState inClientRooms False
    Nothing -> checkState inClientRooms False

 where
  checkState :: Bool -> Bool -> STM Bool
  checkState inClientRooms inServRooms
    | inClientRooms && inServRooms = return True
    | inClientRooms || inServRooms =
      throwSTM (Error $ errorMsg inClientRooms)
    | otherwise = return False

  errorMsg inClientRooms
    | inClientRooms =
      (show clientNick) ++ " is in " ++ (show room) ++
      "for Client record, but not Server record."
    | otherwise =
      (show clientNick) ++ " is in " ++ (show room) ++
      "for Server record, but not Client record."

printToServ :: Server -> String -> STM ()
printToServ Server{..} s =
  writeTChan serverPrintCh s

messageRoom :: Server -> Room -> Message -> STM Bool
messageRoom server@Server{..} room msg = do
  roomMap <- readTVar serverRooms
  case M.lookup room roomMap of
    Nothing -> return False
    Just nicks -> do
      forM_ nicks (\nick -> do
                      mbClient <- findClient server nick
                      case mbClient of
                        Just client ->
                          sendMessage client msg
                        Nothing -> error "Client does not exist")
      return True

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = do
  writeTChan clientSendCh msg

globalMessage :: Server -> Message -> STM ()
globalMessage Server{..} msg = do
  clients <- M.elems <$> readTVar serverClients
  traverse_ (\client -> sendMessage client msg)
    clients

-- Exceptions

data ClientLeft = ClientLeft Client
  deriving Show

data Error = Error String
  deriving Show

instance Exception ClientLeft
instance Exception Error
