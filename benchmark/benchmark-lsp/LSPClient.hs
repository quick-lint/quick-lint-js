-- Copyright 2021 Matthew Glazar
-- See end of file for extended copyright information.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module LSPClient where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Type.Equality as Equality
import qualified LSPDecode as LSP
import qualified Language.LSP.Test as LSP
import qualified Language.LSP.Types as LSP
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO
import qualified System.Timeout as Timeout

data LSPClient =
  LSPClient
    { lspClientServerStdin :: IO.Handle
    , lspClientServerStdout :: IO.Handle
    , lspClientLogTraffic :: Logging
    , lspClientLogLock :: MVar.MVar ()
    , lspClientReadMessagesThread :: Async.Async ()
    , lspClientReadQueue :: Chan.Chan BS.ByteString
    , lspClientRequestMap :: LSP.RequestMap
    , lspClientNextRequestID :: Int
    , lspClientRootPath :: FilePath
    , lspClientServerSyncKind :: LSP.TextDocumentSyncKind
    }

data Logging
  = Quiet
  | LogTraffic IO.Handle

makeLSPClient :: IO.Handle -> IO.Handle -> FilePath -> Logging -> IO LSPClient
makeLSPClient serverStdin serverStdout rootPath logging = do
  logLock <- MVar.newMVar ()
  readQueue <- Chan.newChan
  let readMessagesForever =
        forever $ do
          encodedMessage <- LSP.getNextMessage serverStdout
          logReceive logging logLock encodedMessage
          Chan.writeChan readQueue encodedMessage
  readMessagesThread <-
    Async.async $
    readMessagesForever `Exception.catch` (\LSP.UnexpectedServerTermination -> logEndOfTransmission logging logLock)
  return
    LSPClient
      { lspClientServerStdin = serverStdin
      , lspClientServerStdout = serverStdout
      , lspClientLogTraffic = logging
      , lspClientLogLock = logLock
      , lspClientReadMessagesThread = readMessagesThread
      , lspClientReadQueue = readQueue
      , lspClientRequestMap = LSP.newRequestMap
      , lspClientNextRequestID = 0
      , lspClientRootPath = rootPath
      , lspClientServerSyncKind = LSP.TdSyncNone
      }

receiveMessage :: StateT LSPClient IO LSP.FromServerMessage
receiveMessage = do
  message <- receiveMessageWithTimeout (-1)
  return $ fromJust message

receiveMessageWithTimeout :: Int -> StateT LSPClient IO (Maybe LSP.FromServerMessage)
receiveMessageWithTimeout timeoutMicroseconds = do
  client@LSPClient {..} <- get
  maybeEncodedMessage <- liftIO $ Timeout.timeout timeoutMicroseconds $ Chan.readChan lspClientReadQueue
  case maybeEncodedMessage of
    Just encodedMessage -> do
      let (requestMap', message) = LSP.decodeFromServerMsg lspClientRequestMap encodedMessage
      put client {lspClientRequestMap = requestMap'}
      return $ Just message
    Nothing -> return Nothing

matchResponse ::
     forall (m :: LSP.Method 'LSP.FromClient 'LSP.Request).
     LSP.SMethod m
  -> LSP.LspId m
  -> LSP.FromServerMessage
  -> Maybe (Either LSP.ResponseError (LSP.ResponseResult m))
matchResponse method messageID message =
  case message of
    LSP.FromServerRsp responseMethod (LSP.ResponseMessage _ responseID responseResult) ->
      case method `LSP.mEqClient` responseMethod of
        Just (Right Equality.HRefl)
          | responseID == Just messageID -> Just responseResult
        _ -> Nothing
    _ -> Nothing

matchRequest ::
     forall (m :: LSP.Method 'LSP.FromServer 'LSP.Request). (LSP.Message m ~ LSP.RequestMessage m)
  => LSP.SMethod m
  -> LSP.FromServerMessage
  -> Maybe (LSP.LspId m, LSP.MessageParams m)
matchRequest method message =
  case message of
    LSP.FromServerMess requestMethod request ->
      case method `LSP.mEqServer` requestMethod of
        Just (Right Equality.HRefl) ->
          let LSP.RequestMessage _ messageID _ parameters = request
           in Just (messageID, parameters)
        _ -> Nothing
    _ -> Nothing

matchNotification ::
     forall (m :: LSP.Method 'LSP.FromServer 'LSP.Notification). (LSP.Message m ~ LSP.NotificationMessage m)
  => LSP.SMethod m
  -> LSP.FromServerMessage
  -> Maybe (LSP.MessageParams m)
matchNotification method message =
  case message of
    LSP.FromServerMess notificationMethod notification ->
      case method `LSP.mEqServer` notificationMethod of
        Just (Right Equality.HRefl) ->
          let LSP.NotificationMessage _ _ parameters = notification
           in Just parameters
        _ -> Nothing
    _ -> Nothing

matchAnyNotification :: LSP.FromServerMessage -> Bool
matchAnyNotification =
  \case
    LSP.FromServerMess method message ->
      case LSP.splitServerMethod method of
        LSP.IsServerNot -> True
        LSP.IsServerEither ->
          case message of
            LSP.NotMess _ -> True
            _ -> False
        LSP.IsServerReq -> False
    LSP.FromServerRsp {} -> False

sendRequest ::
     (Aeson.ToJSON (LSP.MessageParams m), Aeson.FromJSON (LSP.SMethod m))
  => LSP.SClientMethod m
  -> LSP.MessageParams m
  -> StateT LSPClient IO (LSP.LspId m)
sendRequest method parameters = do
  client@LSPClient {..} <- get
  let messageID = LSP.IdInt lspClientNextRequestID
  let message = LSP.RequestMessage "2.0" messageID method parameters
  sendEncodedMessage $ Aeson.encode message
  let requestMap' = fromJust $ LSP.updateRequestMap lspClientRequestMap messageID method
  put client {lspClientNextRequestID = lspClientNextRequestID + 1, lspClientRequestMap = requestMap'}
  return messageID

sendResponse :: (Aeson.ToJSON (LSP.ResponseResult m)) => LSP.LspId m -> LSP.ResponseResult m -> StateT LSPClient IO ()
sendResponse messageID result = sendResponseOrError messageID (Right result)

sendResponseOrError ::
     (Aeson.ToJSON (LSP.ResponseResult m))
  => LSP.LspId m
  -> Either LSP.ResponseError (LSP.ResponseResult m)
  -> StateT LSPClient IO ()
sendResponseOrError messageID errorOrResult = sendEncodedMessage $ Aeson.encode message
  where
    message = LSP.ResponseMessage "2.0" (Just messageID) errorOrResult

sendNotification ::
     (Aeson.ToJSON (LSP.MessageParams m))
  => LSP.SClientMethod (m :: LSP.Method 'LSP.FromClient 'LSP.Notification)
  -> LSP.MessageParams m
  -> StateT LSPClient IO ()
sendNotification method parameters = sendEncodedMessage $ Aeson.encode message
  where
    message = LSP.NotificationMessage "2.0" method parameters

sendEncodedMessage :: BS.ByteString -> StateT LSPClient IO ()
sendEncodedMessage encodedMessage = do
  LSPClient {..} <- get
  liftIO $ do
    logSend lspClientLogTraffic lspClientLogLock encodedMessage
    BS.hPut lspClientServerStdin $ LSP.addHeader encodedMessage
    IO.hFlush lspClientServerStdin

logReceive :: Logging -> MVar.MVar () -> BS.ByteString -> IO ()
logReceive logging logLock message =
  case logging of
    LogTraffic handle ->
      lockMVar logLock $ do
        ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
        IO.hPutStrLn handle $ "<~~ recv: " ++ BS.unpack message
        ANSI.hSetSGR handle [ANSI.Reset]
    Quiet -> return ()

logSend :: Logging -> MVar.MVar () -> BS.ByteString -> IO ()
logSend logging logLock message =
  case logging of
    LogTraffic handle ->
      lockMVar logLock $ do
        ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
        IO.hPutStrLn handle $ "~~> send: " ++ BS.unpack message
        ANSI.hSetSGR handle [ANSI.Reset]
    Quiet -> return ()

logEndOfTransmission :: Logging -> MVar.MVar () -> IO ()
logEndOfTransmission logging logLock =
  case logging of
    LogTraffic handle ->
      lockMVar logLock $ do
        ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
        IO.hPutStrLn handle "<~~ end of transmission"
        ANSI.hSetSGR handle [ANSI.Reset]
    Quiet -> return ()

lockMVar :: MVar.MVar a -> IO b -> IO b
lockMVar mvar m = MVar.withMVar mvar (\_value -> m)
-- Copyright 2021 Matthew Glazar
--
-- This file is part of quick-lint-js.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
