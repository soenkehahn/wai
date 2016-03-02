
module Network.Wai.Handler.Warp.WithApplication (
  withApplication,
  openFreePort,
) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Network.Wai.Handler.Warp.Settings
import           Network.Wai.Handler.Warp.Types

data App
  = App {
    appServer :: Async (),
    appExceptionMVar :: MVar (Maybe SomeException),
    appPort :: Int
  }

-- | Runs the given 'Application' on a free port. Passes the port to the given
-- operation and executes it, while the 'Application' is running. Shuts down the
-- server before returning.
--
-- Handy for e.g. testing 'Application's over a real network port.
withApplication :: IO Application -> (Port -> IO a) -> IO a
withApplication mkApp action = do
  app <- mkApp
  bracket (acquire app) free (\ runningApp -> action (appPort runningApp))
  where
    acquire :: Application -> IO App
    acquire app = do
      start <- mkWaiter
      exceptionMVar_ <- newMVar Nothing
      server <- async $ do
        (port, sock) <- openFreePort
        let settings =
              defaultSettings{
                settingsBeforeMainLoop = notify start port
              }
        runSettingsSocket settings sock (handleApp exceptionMVar_ app)
      port <- waitFor start
      return $ App server exceptionMVar_ port

    free :: App -> IO ()
    free runningApp = do
      cancel $ appServer runningApp
      exception <- readMVar (appExceptionMVar runningApp)
      case exception of
        Nothing -> return ()
        Just e -> throwIO e

handleApp :: MVar (Maybe SomeException) -> Application -> Application
handleApp mvar app request respond = do
  catch (app request respond) $ \ e -> do
    modifyMVar_ mvar $ \ _ ->
      return (Just e)
    throwIO e

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return $ Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }

-- | Opens a socket on a free port and returns both port and socket.
openFreePort :: IO (Port, Socket)
openFreePort = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
