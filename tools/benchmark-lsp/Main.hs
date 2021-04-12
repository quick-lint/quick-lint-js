-- Copyright 2021 Matthew Glazar
-- See end of file for extended copyright information.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import qualified Criterion.Main as Criterion
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function (fix)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import LSPBenchmark
import qualified LSPClient
import qualified Language.LSP.Types as LSP
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO

main :: IO ()
main = do
  benchmarkConfigOrError <- Aeson.eitherDecodeFileStrict "benchmark-config.json" :: IO (Either String BenchmarkConfig)
  benchmarkConfig <-
    case benchmarkConfigOrError of
      Left errorMessage -> do
        IO.hPutStrLn IO.stderr errorMessage
        Exit.exitFailure
      Right benchmarkConfig -> return benchmarkConfig
  Criterion.defaultMain $ map benchmarkLSPServer (benchmarkConfigServers benchmarkConfig)

benchmarkLSPServer :: BenchmarkConfigServer -> Criterion.Benchmark
benchmarkLSPServer serverConfig@BenchmarkConfigServer {..} =
  Criterion.bgroup benchmarkConfigServerName [benchOpenWaitClose serverConfig, benchChangeWait serverConfig]

-- TODO(strager): Add a similar benchmark with a warmup phase.
benchOpenWaitClose :: BenchmarkConfigServer -> Criterion.Benchmark
benchOpenWaitClose serverConfig = benchmarkWithLSPServer "open-wait-close" serverConfig setUp run
  where
    setUp :: Int64 -> StateT LSPClient.LSPClient IO (Int64, LSP.Uri)
    setUp batchSize = do
      uri <- makeURI "test.js"
      createFileOnDiskIfNeeded serverConfig uri
      return (batchSize, uri)
    run :: (Int64, LSP.Uri) -> StateT LSPClient.LSPClient IO ()
    run (batchSize, uri) =
      forM_ [1 .. batchSize] $ \i
        -- NOTE(strager): Deno expects version numbers to be unique, even
        -- after closing and reopening a document.
       -> do
        let version = fromIntegral i :: Int
        sendTextDocumentDidOpenNotification uri version "javascript" source
        fix $ \loop ->
          waitForDiagnosticsOrTimeout (secondsToMicroseconds 10) >>= \case
            Nothing -> fail "Expected diagnostics but received none"
            -- HACK(strager): After sending textDocument/didClose, eslint-server
            -- gives us a textDocument/publishDiagnostics message with no
            -- diagnostics. Make sure we skip these bogus notifications.
            Just (LSP.PublishDiagnosticsParams _ _ (LSP.List [])) -> loop
            Just _diagnostics -> return ()
        LSPClient.sendNotification LSP.STextDocumentDidClose $
          LSP.DidCloseTextDocumentParams (LSP.TextDocumentIdentifier uri)
    source :: Text.Text
    source = "let x, x;"

benchChangeWait :: BenchmarkConfigServer -> Criterion.Benchmark
benchChangeWait serverConfig@BenchmarkConfigServer {..} = benchmarkWithLSPServer "change-wait" serverConfig setUp run
  where
    setUp :: Int64 -> StateT LSPClient.LSPClient IO [LSP.Uri]
    setUp batchSize = do
      documentURIs <-
        forM [1 .. batchSize] $ \i -> do
          uri <- makeURI ("test" <> show i <> ".js")
          createFileOnDiskIfNeeded serverConfig uri
          let version = 0
          sendTextDocumentDidOpenNotification uri version "javascript" initialSource
          return uri
      when benchmarkConfigServerWaitForEmptyDiagnosticsOnOpen $ waitForAllDiagnostics (HashSet.fromList documentURIs)
      return documentURIs
    run :: [LSP.Uri] -> StateT LSPClient.LSPClient IO ()
    run documentURIs =
      forM_ documentURIs $ \uri -> do
        let version = 1
        sendTextDocumentDidFullyChangeNotification uri version modifiedSource
        fix $ \loop ->
          waitForDiagnosticsOrTimeout (secondsToMicroseconds 10) >>= \case
            Just (LSP.PublishDiagnosticsParams diagnosticsURI _ (LSP.List diagnostics))
              | diagnosticsURI == uri ->
                if null diagnostics
                  -- Perhaps this notification is from a previous change to a
                  -- different document. Ignore it.
                  then loop
                  else return ()
              -- This notification is for a different document. (Some LSP
              -- servers send notifications for all documents after any document
              -- changes.) Ignore it.
              | otherwise -> loop
            Nothing -> fail "Expected diagnostics but received none"
    initialSource :: Text.Text
    initialSource = ""
    modifiedSource :: Text.Text
    modifiedSource = "let x, x;"

createFileOnDiskIfNeeded :: BenchmarkConfigServer -> LSP.Uri -> StateT LSPClient.LSPClient IO ()
createFileOnDiskIfNeeded BenchmarkConfigServer {..} uri =
  when benchmarkConfigServerNeedFilesOnDisk $ liftIO $ BS.writeFile filePath BS.empty
  where
    filePath = fromJust (LSP.uriToFilePath uri)

makeURI :: FilePath -> StateT LSPClient.LSPClient IO LSP.Uri
makeURI relativePath = LSP.filePathToUri <$> makeFilePath relativePath

makeFilePath :: FilePath -> StateT LSPClient.LSPClient IO FilePath
makeFilePath relativePath = do
  rootPath <- gets LSPClient.lspClientRootPath
  return $ rootPath </> relativePath

sendTextDocumentDidOpenNotification :: LSP.Uri -> Int -> Text.Text -> Text.Text -> StateT LSPClient.LSPClient IO ()
sendTextDocumentDidOpenNotification uri version language text =
  LSPClient.sendNotification LSP.STextDocumentDidOpen $
  LSP.DidOpenTextDocumentParams $ LSP.TextDocumentItem uri language version text

sendTextDocumentDidFullyChangeNotification :: LSP.Uri -> Int -> Text.Text -> StateT LSPClient.LSPClient IO ()
sendTextDocumentDidFullyChangeNotification uri version text = do
  let changes = [LSP.TextDocumentContentChangeEvent Nothing Nothing text]
  LSPClient.sendNotification LSP.STextDocumentDidChange $
    LSP.DidChangeTextDocumentParams (LSP.VersionedTextDocumentIdentifier uri (Just version)) (LSP.List changes)

waitForDiagnosticsOrTimeout :: Int -> StateT LSPClient.LSPClient IO (Maybe LSP.PublishDiagnosticsParams)
waitForDiagnosticsOrTimeout timeoutMicroseconds =
  fix $ \loop ->
    LSPClient.receiveMessageWithTimeout timeoutMicroseconds >>= \case
      Just (LSPClient.matchNotification LSP.STextDocumentPublishDiagnostics -> Just parameters) ->
        return $ Just parameters
      Just (matchMiscMessage -> Just handle) -> handle >> loop
      Nothing -> return Nothing
      _ -> fail "Unimplemented message"

waitForAllDiagnostics :: HashSet.HashSet LSP.Uri -> StateT LSPClient.LSPClient IO ()
waitForAllDiagnostics urisNeedingDiagnostics
  | HashSet.null urisNeedingDiagnostics = return ()
  | otherwise =
    waitForDiagnosticsOrTimeout (secondsToMicroseconds 1) >>= \case
      Just (LSP.PublishDiagnosticsParams diagnosticsURI _ diagnostics)
        | diagnosticsURI `HashSet.member` urisNeedingDiagnostics ->
          if null diagnostics
            then waitForAllDiagnostics (HashSet.delete diagnosticsURI urisNeedingDiagnostics)
            else fail "Expected no diagnostics but received some"
        | otherwise -> waitForAllDiagnostics urisNeedingDiagnostics
      Nothing -> return ()
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
