module Main where
 
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
 
main :: IO ()
main = do
 sock <- socket AF_INET Stream 0
 setSocketOption sock ReuseAddr 1
 bind sock (SockAddrInet 4242 iNADDR_ANY)
 listen sock 2
 chan <- newChan
 forkIO $ readingChan chan
 mainLoop sock chan 0
 
type Msg = (Int, String)

readingChan :: Chan a -> IO b
readingChan chan = readChan chan >> readingChan chan

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
 conn <- accept sock
 forkIO $ runConn conn chan msgNum
 mainLoop sock chan $! msgNum + 1
 
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
 hdl <- initSocket sock
 broadcast <- initBroadcast chan msgNum
 name <- gettingUserName hdl broadcast
 commLine <- dupChan chan
 reader <- readChannelInThread commLine msgNum hdl
 handle (\(SomeException _) -> return ()) $ readAndBroadcast hdl broadcast name
 finishing reader broadcast name hdl

finishing :: ThreadId -> (String -> IO a) -> String -> Handle -> IO ()
finishing reader broadcast name hdl = do
 killThread reader                      -- kill after the loop ends
 broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
 hClose hdl

initBroadcast :: Monad m => Chan Msg -> Int -> m (String -> IO ())
initBroadcast chan msgNum = do
 let broadcast _ = writeChan chan (msgNum, "")
 return broadcast

initSocket :: Socket -> IO Handle
initSocket sock = do
 hdl <- socketToHandle sock ReadWriteMode
 hSetBuffering hdl NoBuffering
 return hdl

gettingUserName :: Handle -> (String -> IO a) -> IO String
gettingUserName hdl broadcast = do
 hPutStrLn hdl "Hi, what's your name?"
 name <- readSocketLine hdl
 broadcast ("--> " ++ name ++ " entered chat.")
 hPutStrLn hdl ("Welcome, " ++ name ++ "!")
 return name

readAndBroadcast :: Handle -> (String -> IO a) -> String -> IO ()
readAndBroadcast hdl broadcast name = do
 line <- readSocketLine hdl
 case line of
  -- If an exception is caught, send a message and break the loop
  "quit" -> hPutStrLn hdl "Bye!"
  -- else, continue looping.
  _ -> broadcastAndContinue broadcast name line hdl

broadcastAndContinue :: (String -> IO a) -> String -> String -> Handle -> IO ()
broadcastAndContinue broadcast name line hdl = do
 broadcast (name ++ ": " ++ line)
 readAndBroadcast hdl broadcast name

readChannelInThread :: Chan Msg -> Int -> Handle -> (IO ThreadId)
readChannelInThread commLine msgNum hdl = forkIO $ readChannel msgNum commLine hdl

readChannel :: Eq t => t -> Chan (t, String) -> Handle -> IO b
readChannel msgNum commLine hdl = do
 (nextNum, line) <- readChan commLine
 when (msgNum /= nextNum) $ hPutStrLn hdl line
 readChannel msgNum commLine hdl

readSocketLine :: Handle -> IO String
readSocketLine hdl = liftM init $ hGetLine hdl