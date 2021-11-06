-- Copyright 2021 Matthew "strager" Glazar
-- See end of file for extended copyright information.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LSPBenchmark where

import qualified Control.Concurrent.Async as Async
import Control.DeepSeq (NFData(rnf))
import Control.Lens ((^.))
import Control.Monad.Trans.State.Strict
import qualified Criterion.Main as Criterion
import qualified Criterion.Measurement.Types as Criterion
import qualified Data.Aeson as Aeson
import Data.Default (def)
import Data.Dynamic (Typeable)
import Data.Function (fix)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified LSPClient
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Capabilities as LSP
import qualified Language.LSP.Types.Lens as LSP hiding (message)
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process
import qualified System.Timeout as Timeout

-- | Create a benchmark which times some work after setting up an LSP server.
benchmarkWithLSPServer ::
     forall a env. (NFData a, NFData env, Typeable a, Typeable env)
  => String
  -- ^ Benchmark name.
  -> BenchmarkConfigServer
  -- ^ LSP server configuration.
  -> (Int64 -> StateT LSPClient.LSPClient IO env)
  -- ^ Setup. Given batch size.
  -> (env -> StateT LSPClient.LSPClient IO a)
  -- ^ Work. Given env returned by setup.
  -> Criterion.Benchmark
benchmarkWithLSPServer name serverConfig setUp work =
  Criterion.bench name $ Criterion.Benchmarkable alloc clean work' cleanUpEachRun
  where
    alloc :: Int64 -> IO (LSPServerBenchmarkEnv env)
    alloc batchSize = do
      (lspClient, lspServerProcess) <- spawnLSPServerForBenchmarking serverConfig
      (env, lspClient') <- runStateT (setUp batchSize) lspClient
      lspClientIORef <- IORef.newIORef lspClient'
      return $ LSPServerBenchmarkEnv env lspClientIORef lspServerProcess
    work' :: LSPServerBenchmarkEnv env -> Int64 -> IO ()
    work' ~(LSPServerBenchmarkEnv env lspClientIORef _lspServerProcess) _batchSize = do
      lspClient <- IORef.readIORef lspClientIORef
      (result, lspClient') <- runStateT (work env) lspClient
      IORef.writeIORef lspClientIORef lspClient'
      rnf result `seq` return ()
    clean :: Int64 -> LSPServerBenchmarkEnv env -> IO ()
    clean _batchSize (LSPServerBenchmarkEnv _env lspClientIORef lspServerProcess) = do
      lspClient <- IORef.readIORef lspClientIORef
      shutDownLSPServer lspClient serverConfig lspServerProcess
    cleanUpEachRun = False

data LSPServerBenchmarkEnv userEnv =
  LSPServerBenchmarkEnv
    { lspServerBenchmarkEnvUserEnv :: userEnv
    , lspServerBenchmarkEnvClient :: IORef.IORef LSPClient.LSPClient
    , lspServerBenchmarkEnvServer :: SpawnedProcess
    }
  deriving (Generic, NFData)

spawnLSPServerForBenchmarking :: BenchmarkConfigServer -> IO (LSPClient.LSPClient, SpawnedProcess)
spawnLSPServerForBenchmarking BenchmarkConfigServer {..} = do
  logging <- getLoggingOptions
  (lspClient, lspServerProcess) <-
    spawnLSPServer
      (Process.proc (head benchmarkConfigServerCommand) (tail benchmarkConfigServerCommand))
        {Process.cwd = benchmarkConfigServerCWD, Process.std_err = Process.Inherit}
      logging
  lspClient' <- execStateT (initializeLSP benchmarkConfigServerInitializationOptions) lspClient
  return (lspClient', lspServerProcess)

spawnLSPServer :: Process.CreateProcess -> LSPClient.Logging -> IO (LSPClient.LSPClient, SpawnedProcess)
spawnLSPServer proc logging = do
  cwd <- Directory.getCurrentDirectory
  let rootPath = cwd </> fromMaybe "." (Process.cwd proc)
  lspServerProcess@(Just lspServerStdin, Just lspServerStdout, _lspServerStderr, _lspServerHandle) <-
    Process.createProcess proc {Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe}
  lspClient <- LSPClient.makeLSPClient lspServerStdin lspServerStdout rootPath logging
  return (lspClient, lspServerProcess)

getLoggingOptions :: IO LSPClient.Logging
getLoggingOptions = do
  debug <- Environment.lookupEnv "QLJS_BENCHMARK_LSP_DEBUG"
  return $
    case debug of
      Just "" -> LSPClient.Quiet
      Just "0" -> LSPClient.Quiet
      Nothing -> LSPClient.Quiet
      Just _value -> LSPClient.LogTraffic IO.stderr

type SpawnedProcess = (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle, Process.ProcessHandle)

shutDownLSPServer :: LSPClient.LSPClient -> BenchmarkConfigServer -> SpawnedProcess -> IO ()
shutDownLSPServer lspClient serverConfig lspServerProcess@(_, _, _, lspServerProcessHandle) = do
  lspClient' <- execStateT (shutDownLSP serverConfig) lspClient
  -- Some servers don't implement 'exit', so terminate the server manually.
  Process.terminateProcess lspServerProcessHandle
  _ <- Timeout.timeout (secondsToMicroseconds 1) $ Process.waitForProcess lspServerProcessHandle
  Process.cleanupProcess lspServerProcess
  Async.wait $ LSPClient.lspClientReadMessagesThread lspClient'

initializeLSP ::
     Aeson.Value
  -- ^ Initialization options.
  -> StateT LSPClient.LSPClient IO ()
initializeLSP initializationOptions = do
  rootPath <- gets LSPClient.lspClientRootPath
  -- Flow's LSP server requires rootURI.
  let rootURI = LSP.filePathToUri rootPath
  let workspaceFolders = [LSP.WorkspaceFolder (LSP.getUri rootURI) "benchmarks"]
  -- PublishDiagnosticsClientCapabilities is required by the TypeScript LSP
  -- server since version 0.6.0:
  -- https://github.com/typescript-language-server/typescript-language-server/pull/229
  let textDocumentClientCapabilities =
        def {LSP._publishDiagnostics = Just (LSP.PublishDiagnosticsClientCapabilities Nothing Nothing Nothing)}
  let clientCapabilities = LSP.ClientCapabilities Nothing (Just textDocumentClientCapabilities) Nothing Nothing
  initializeID <-
    LSPClient.sendRequest LSP.SInitialize $
    LSP.InitializeParams
      Nothing
      Nothing
      Nothing
      (Just (Text.pack rootPath))
      (Just rootURI)
      (Just initializationOptions)
      clientCapabilities
      Nothing
      (Just (LSP.List workspaceFolders))
  initializeResult <-
    fix $ \loop ->
      LSPClient.receiveMessage >>= \case
        (LSPClient.matchResponse LSP.SInitialize initializeID -> Just (Right initializeResult)) ->
          return initializeResult
        (LSPClient.matchAnyNotification -> True) -> loop
        _ -> fail "Unimplemented message"
  textDocumentSyncKind <- checkTextDocumentSyncKind initializeResult
  modify $ \client -> client {LSPClient.lspClientServerSyncKind = textDocumentSyncKind}
  LSPClient.sendNotification LSP.SInitialized $ Just LSP.InitializedParams

checkTextDocumentSyncKind :: LSP.InitializeResult -> StateT LSPClient.LSPClient IO LSP.TextDocumentSyncKind
checkTextDocumentSyncKind initializeResult = do
  case textDocumentSyncKind of
    LSP.TdSyncNone -> fail "LSP server does not support document syncing"
    LSP.TdSyncIncremental -> return ()
    LSP.TdSyncFull -> return ()
  return textDocumentSyncKind
  where
    textDocumentSyncKind =
      case initializeResult ^. LSP.capabilities ^. LSP.textDocumentSync of
        Just (LSP.InR kind) -> kind
        Just (LSP.InL ((^. LSP.change) -> Just kind)) -> kind
        _ -> LSP.TdSyncNone

shutDownLSP :: BenchmarkConfigServer -> StateT LSPClient.LSPClient IO ()
shutDownLSP serverConfig = do
  shutdownID <- LSPClient.sendRequest LSP.SShutdown LSP.Empty
  fix $ \loop ->
    LSPClient.receiveMessage >>= \case
      (LSPClient.matchResponse LSP.SShutdown shutdownID -> Just (Right _)) -> return ()
      (matchMiscMessage serverConfig -> Just handle) -> handle >> loop
      _ -> fail "Unimplemented message"
  LSPClient.sendNotification LSP.SExit LSP.Empty

matchMiscMessage :: BenchmarkConfigServer -> LSP.FromServerMessage -> Maybe (StateT LSPClient.LSPClient IO ())
matchMiscMessage BenchmarkConfigServer {..} =
  \case
    (LSPClient.matchRequest LSP.SClientRegisterCapability -> Just (requestID, _request)) ->
      Just $ do LSPClient.sendResponse requestID LSP.Empty
    (LSPClient.matchRequest LSP.SWorkspaceConfiguration -> Just (requestID, request)) ->
      Just $ LSPClient.sendResponse requestID (LSP.List configurations)
      where (LSP.List requestedItems) = request ^. LSP.items
            configurations = map (\_item -> benchmarkConfigServerWorkspaceConfiguration) requestedItems
    (LSPClient.matchAnyNotification -> True) -> Just $ return ()
    _ -> Nothing

secondsToMicroseconds :: Int -> Int
secondsToMicroseconds seconds = seconds * 1000 * 1000

data BenchmarkConfig =
  BenchmarkConfig
    { benchmarkConfigServers :: [BenchmarkConfigServer]
    }

data BenchmarkConfigServer =
  BenchmarkConfigServer
    { benchmarkConfigServerName :: String
    , benchmarkConfigServerCommand :: [String]
    , benchmarkConfigServerCWD :: Maybe FilePath
    , benchmarkConfigServerAllowIncrementalChanges :: Bool
    , benchmarkConfigServerBrokenBenchmarkNames :: [String]
    , benchmarkConfigServerDiagnosticsMessagesToIgnore :: Int
    , benchmarkConfigServerInitializationOptions :: Aeson.Value
    , benchmarkConfigServerNeedFilesOnDisk :: Bool
    , benchmarkConfigServerWaitForEmptyDiagnosticsOnOpen :: Bool
    , benchmarkConfigServerWorkspaceConfiguration :: Aeson.Value
    }

instance Aeson.FromJSON BenchmarkConfig where
  parseJSON = Aeson.withObject "BenchmarkConfig" $ \v -> BenchmarkConfig <$> v Aeson..: "servers"

instance Aeson.FromJSON BenchmarkConfigServer where
  parseJSON =
    Aeson.withObject "BenchmarkConfigServer" $ \v ->
      BenchmarkConfigServer <$> v Aeson..: "name" <*> v Aeson..: "command" <*> v Aeson..:? "cwd" <*>
      (fromMaybe True <$> v Aeson..:? "allowIncrementalChanges") <*>
      (fromMaybe [] <$> v Aeson..:? "broken") <*>
      (fromMaybe 0 <$> v Aeson..:? "diagnosticsMessagesToIgnore") <*>
      (fromMaybe (Aeson.Object HashMap.empty) <$> v Aeson..:? "initializationOptions") <*>
      (fromMaybe False <$> v Aeson..:? "needFilesOnDisk") <*>
      (fromMaybe True <$> v Aeson..:? "waitForEmptyDiagnosticsOnOpen") <*>
      (fromMaybe (Aeson.Object HashMap.empty) <$> v Aeson..:? "workspaceConfiguration")

instance NFData IO.Handle where
  rnf x = x `seq` ()

instance NFData Process.ProcessHandle where
  rnf x = x `seq` ()
-- Copyright 2021 Matthew "strager" Glazar
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
