module Client where

--import ByteString
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import System.IO
import System.Exit
import Network

port :: Int
port = 23456

main :: IO ()
main = withSocketsDo $ do

  printCh <- newChan

  handle <- connectTo "localhost" (PortNumber (fromIntegral port))
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

  forkIO $ forever $ do
    msg <- readChan printCh
    putStrLn msg

  forkIO $ forever $ do
    msg <- getLine
    hPutStr handle $ msg ++ "\n"

  catch
    (forever $ do
      msg <- hGetLine handle
      writeChan printCh msg)
    (\(SomeException _) ->
       putStrLn "Lost connection to server.")

  return ()
