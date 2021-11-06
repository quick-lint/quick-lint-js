-- Copyright 2021 Matthew "strager" Glazar
-- See end of file for extended copyright information.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State.Strict
import qualified Criterion.Main as Criterion
import qualified Criterion.Measurement.Types as Criterion
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Dynamic (Typeable)
import Data.Function (fix)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import qualified Data.Maybe
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import LSPBenchmark
import qualified LSPClient
import qualified Language.LSP.Types as LSP
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Text.Printf as Printf

main :: IO ()
main = do
  benchmarkConfigOrError <- Aeson.eitherDecodeFileStrict "benchmark-config.json" :: IO (Either String BenchmarkConfig)
  benchmarkConfig <-
    case benchmarkConfigOrError of
      Left errorMessage -> do
        IO.hPutStrLn IO.stderr errorMessage
        Exit.exitFailure
      Right benchmarkConfig -> return benchmarkConfig
  corpus <- loadCorpus
  Criterion.defaultMain $ map (benchmarkLSPServer corpus) (benchmarkConfigServers benchmarkConfig)

loadCorpus :: IO JavaScriptCorpus
loadCorpus =
  JavaScriptCorpus <$> Text.readFile "corpus/tiny.js" <*> Text.readFile "corpus/edex-ui-filesystem.class.js" <*>
  Text.readFile "corpus/express-router.js"

data JavaScriptCorpus =
  JavaScriptCorpus
    { javaScriptCorpusTiny :: !Text.Text
    , javaScriptCorpusEdexUIFilesystemClassJS :: !Text.Text
    , javaScriptCorpusExpressRouterJS :: !Text.Text
    }

benchmarkLSPServer :: JavaScriptCorpus -> BenchmarkConfigServer -> Criterion.Benchmark
benchmarkLSPServer JavaScriptCorpus {..} serverConfig@BenchmarkConfigServer {..} =
  Criterion.bgroup benchmarkConfigServerName $
  removeBrokenBenchmarks
    [ Criterion.bgroup
        "open-wait-close"
        [ benchOpenWaitClose javaScriptCorpusTiny "tiny.js" serverConfig
        , benchOpenWaitClose javaScriptCorpusEdexUIFilesystemClassJS "edex-ui-filesystem.class.js" serverConfig
        , benchOpenWaitClose javaScriptCorpusExpressRouterJS "express-router.js" serverConfig
        ]
    , Criterion.bgroup
        "change-wait"
        [ benchChangeWait javaScriptCorpusTiny "tiny.js" serverConfig
        , benchChangeWait javaScriptCorpusEdexUIFilesystemClassJS "edex-ui-filesystem.class.js" serverConfig
        , benchChangeWait javaScriptCorpusExpressRouterJS "express-router.js" serverConfig
        ]
    , bgroupIf
        benchmarkConfigServerAllowIncrementalChanges
        "incremental-change-wait"
        [ benchIncrementalChangeWait
            javaScriptCorpusExpressRouterJS
            "express-router.js"
            expressRouterJSChanges
            serverConfig
        ]
    , Criterion.bgroup
        "full-change-wait"
        [benchFullChangeWait javaScriptCorpusExpressRouterJS "express-router.js" modifyExpressRouterJS serverConfig]
    ]
      -- | In the "create Router#VERB functions" arrow function in
      -- express-router.js, replace 'method' (declaration and references) with
      -- 'm00001', then 'm00002', etc.
  where
    removeBrokenBenchmarks :: [Criterion.Benchmark] -> [Criterion.Benchmark]
    removeBrokenBenchmarks = map (filterBenchmarks isBenchmarkGood)
      where
        isBenchmarkGood benchmarkName = not (benchmarkName `elem` benchmarkConfigServerBrokenBenchmarkNames)
    expressRouterJSChanges i = [makeChange 506 39, makeChange 507 8, makeChange 509 10]
      where
        newVariableName = Text.pack $ Printf.printf "m%05d" i
        makeChange line startColumn =
          LSP.TextDocumentContentChangeEvent
            (Just $
             LSP.Range (LSP.Position line startColumn) (LSP.Position line (startColumn + Text.length newVariableName)))
            Nothing
            newVariableName
    modifyExpressRouterJS i =
      Text.decodeUtf8 $
      changeAt 11854 $ changeAt 11871 $ changeAt 11940 $ Text.encodeUtf8 javaScriptCorpusExpressRouterJS
      where
        newVariableName = Text.encodeUtf8 $ Text.pack $ Printf.printf "m%05d" i
        changeAt byteOffset bytes = before <> newVariableName <> after
          where
            (before, oldVariableNameAndAfter) = BS.splitAt byteOffset bytes
            (_oldVariableName, after) = BS.splitAt (BS.length newVariableName) oldVariableNameAndAfter

-- TODO(strager): Add a similar benchmark with a warmup phase.
benchOpenWaitClose :: Text.Text -> String -> BenchmarkConfigServer -> Criterion.Benchmark
benchOpenWaitClose fileContent benchmarkName serverConfig = benchmarkWithLSPMServer benchmarkName serverConfig setUp run
  where
    setUp :: Int64 -> LSPM (Int64, LSP.Uri)
    setUp batchSize = do
      uri <- makeURI "test.js"
      createFileOnDiskIfNeeded serverConfig uri
      return (batchSize, uri)
    run :: (Int64, LSP.Uri) -> LSPM ()
    run (batchSize, uri) =
      forM_ [1 .. batchSize] $ \i
        -- NOTE(strager): Deno expects version numbers to be unique, even
        -- after closing and reopening a document.
       -> do
        let version = fromIntegral i :: Int
        sendTextDocumentDidOpenNotification uri version "javascript" fileContent
        fix $ \loop ->
          waitForDiagnosticsOrTimeout (secondsToMicroseconds 10) >>= \case
            Nothing -> fail "Expected diagnostics but received none"
            -- HACK(strager): After sending textDocument/didClose, eslint-server
            -- gives us a textDocument/publishDiagnostics message with no
            -- diagnostics. Make sure we skip these bogus notifications.
            Just (LSP.PublishDiagnosticsParams _ _ (LSP.List [])) -> loop
            Just _diagnostics -> return ()
        liftLSP $
          LSPClient.sendNotification LSP.STextDocumentDidClose $
          LSP.DidCloseTextDocumentParams (LSP.TextDocumentIdentifier uri)

benchChangeWait :: Text.Text -> String -> BenchmarkConfigServer -> Criterion.Benchmark
benchChangeWait modifiedSource benchmarkName serverConfig@BenchmarkConfigServer {..} =
  benchmarkWithLSPMServer benchmarkName serverConfig setUp run
  where
    setUp :: Int64 -> LSPM [LSP.Uri]
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
    run :: [LSP.Uri] -> LSPM ()
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

benchIncrementalChangeWait ::
     Text.Text
  -> String
  -> (Int64 -> [LSP.TextDocumentContentChangeEvent])
  -> BenchmarkConfigServer
  -> Criterion.Benchmark
benchIncrementalChangeWait originalSource benchmarkName makeChanges serverConfig =
  benchmarkWithLSPMServer benchmarkName serverConfig setUp run
  where
    setUp :: Int64 -> LSPM (LSP.Uri, Int64, [LSP.Diagnostic])
    setUp batchSize = do
      requireServerIncrementalSyncSupport
      uri <- makeURI "test.js"
      createFileOnDiskIfNeeded serverConfig uri
      let version = 0
      sendTextDocumentDidOpenNotification uri version "javascript" originalSource
      diagnostics <- waitUntilSomeDiagnosticsWithTimeout (secondsToMicroseconds 10)
      return (uri, batchSize, diagnostics)
    run :: (LSP.Uri, Int64, [LSP.Diagnostic]) -> LSPM ()
    run (uri, batchSize, expectedDiagnostics) =
      forM_ [1 .. batchSize] $ \i -> do
        let version = fromIntegral i :: Int
        let changes = makeChanges (i - 1)
        liftLSP $
          LSPClient.sendNotification LSP.STextDocumentDidChange $
          LSP.DidChangeTextDocumentParams (LSP.VersionedTextDocumentIdentifier uri (Just version)) (LSP.List changes)
        waitForDiagnosticsOrTimeout (secondsToMicroseconds 1) >>= \case
          Nothing -> fail "Expected diagnostics but received none"
          Just (LSP.PublishDiagnosticsParams _ _ (LSP.List diagnostics)) ->
            unless (length diagnostics == length expectedDiagnostics) $
            fail $
            "Diagnostics unexpectedly changed:\nExpected: " ++
            show expectedDiagnostics ++ "\nReceived: " ++ show diagnostics

benchFullChangeWait :: Text.Text -> String -> (Int64 -> Text.Text) -> BenchmarkConfigServer -> Criterion.Benchmark
benchFullChangeWait originalSource benchmarkName makeModifiedSource serverConfig =
  benchmarkWithLSPMServer benchmarkName serverConfig setUp run
  where
    setUp :: Int64 -> LSPM (LSP.Uri, Int64, [LSP.Diagnostic])
    setUp batchSize = do
      uri <- makeURI "test.js"
      createFileOnDiskIfNeeded serverConfig uri
      let version = 0
      sendTextDocumentDidOpenNotification uri version "javascript" originalSource
      diagnostics <- waitUntilSomeDiagnosticsWithTimeout (secondsToMicroseconds 10)
      return (uri, batchSize, diagnostics)
    run :: (LSP.Uri, Int64, [LSP.Diagnostic]) -> LSPM ()
    run (uri, batchSize, expectedDiagnostics) =
      forM_ [1 .. batchSize] $ \i -> do
        let version = fromIntegral i :: Int
        let modifiedSource = makeModifiedSource (i - 1)
        sendTextDocumentDidFullyChangeNotification uri version modifiedSource
        diagnostics <- waitUntilSomeDiagnosticsWithTimeout (secondsToMicroseconds 1)
        unless (length diagnostics == length expectedDiagnostics) $
          fail $
          "Diagnostics unexpectedly changed:\nExpected: " ++
          show expectedDiagnostics ++ "\nReceived: " ++ show diagnostics

-- LSP monad.
newtype LSPM a =
  LSPM
    { unLSPM :: ReaderT BenchmarkConfigServer (StateT LSPClient.LSPClient IO) a
    }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

liftLSP :: StateT LSPClient.LSPClient IO a -> LSPM a
liftLSP m = LSPM (lift m)

-- | Like benchmarkWithLSPServer, but for LSPM.
benchmarkWithLSPMServer ::
     forall a env. (NFData a, NFData env, Typeable a, Typeable env)
  => String
  -- ^ Benchmark name.
  -> BenchmarkConfigServer
  -- ^ LSP server configuration.
  -> (Int64 -> LSPM env)
  -- ^ Setup. Given batch size.
  -> (env -> LSPM a)
  -- ^ Work. Given env returned by setup.
  -> Criterion.Benchmark
benchmarkWithLSPMServer name serverConfig setUp work =
  benchmarkWithLSPServer
    name
    serverConfig
    (\batchSize -> runReaderT (unLSPM $ setUp batchSize) serverConfig)
    (\env -> runReaderT (unLSPM $ work env) serverConfig)

requireServerIncrementalSyncSupport :: LSPM ()
requireServerIncrementalSyncSupport = do
  textDocumentSyncKind <- liftLSP $ gets LSPClient.lspClientServerSyncKind
  case textDocumentSyncKind of
    LSP.TdSyncNone -> fail "LSP server does not support document syncing"
    LSP.TdSyncIncremental -> return ()
    LSP.TdSyncFull -> fail "LSP server does not support incremental document syncing"

createFileOnDiskIfNeeded :: BenchmarkConfigServer -> LSP.Uri -> LSPM ()
createFileOnDiskIfNeeded BenchmarkConfigServer {..} uri =
  when benchmarkConfigServerNeedFilesOnDisk $ liftIO $ BS.writeFile filePath BS.empty
  where
    filePath = fromJust (LSP.uriToFilePath uri)

makeURI :: FilePath -> LSPM LSP.Uri
makeURI relativePath = LSP.filePathToUri <$> makeFilePath relativePath

makeFilePath :: FilePath -> LSPM FilePath
makeFilePath relativePath = do
  rootPath <- liftLSP $ gets LSPClient.lspClientRootPath
  return $ rootPath </> relativePath

sendTextDocumentDidOpenNotification :: LSP.Uri -> Int -> Text.Text -> Text.Text -> LSPM ()
sendTextDocumentDidOpenNotification uri version language text =
  liftLSP $
  LSPClient.sendNotification LSP.STextDocumentDidOpen $
  LSP.DidOpenTextDocumentParams $ LSP.TextDocumentItem uri language version text

sendTextDocumentDidFullyChangeNotification :: LSP.Uri -> Int -> Text.Text -> LSPM ()
sendTextDocumentDidFullyChangeNotification uri version text = do
  let changes = [LSP.TextDocumentContentChangeEvent Nothing Nothing text]
  liftLSP $
    LSPClient.sendNotification LSP.STextDocumentDidChange $
    LSP.DidChangeTextDocumentParams (LSP.VersionedTextDocumentIdentifier uri (Just version)) (LSP.List changes)

waitForDiagnosticsOrTimeout :: Int -> LSPM (Maybe LSP.PublishDiagnosticsParams)
waitForDiagnosticsOrTimeout timeoutMicroseconds = do
  diagnosticsMessagesToIgnore <- LSPM $ asks benchmarkConfigServerDiagnosticsMessagesToIgnore
  let go seenDiagnosticMessages = do
        maybeDiagnostics <- waitForOneDiagnosticsMessageOrTimeout timeoutMicroseconds
        case maybeDiagnostics of
          Just diagnostics
            | length seenDiagnosticMessages < diagnosticsMessagesToIgnore -> go (diagnostics : seenDiagnosticMessages)
          _ -> return maybeDiagnostics
  go []

waitForOneDiagnosticsMessageOrTimeout :: Int -> LSPM (Maybe LSP.PublishDiagnosticsParams)
waitForOneDiagnosticsMessageOrTimeout timeoutMicroseconds = do
  serverConfig <- LSPM ask
  fix $ \loop ->
    liftLSP (LSPClient.receiveMessageWithTimeout timeoutMicroseconds) >>= \case
      Just (LSPClient.matchNotification LSP.STextDocumentPublishDiagnostics -> Just parameters) ->
        return $ Just parameters
      Just (matchMiscMessage serverConfig -> Just handle) -> liftLSP handle >> loop
      Nothing -> return Nothing
      _ -> fail "Unimplemented message"

-- | HACK(strager): Some LSP servers, such as Flow and TypeScript, give us an
-- empty list of diagnostics before giving us the real list of diagnostics. Skip
-- the empty list and wait for the real list.
waitUntilSomeDiagnosticsWithTimeout :: Int -> LSPM [LSP.Diagnostic]
waitUntilSomeDiagnosticsWithTimeout timeoutMicroseconds =
  fix $ \loop ->
    waitForDiagnosticsOrTimeout timeoutMicroseconds >>= \case
      Just (LSP.PublishDiagnosticsParams _ _ (LSP.List [])) -> loop
      Just (LSP.PublishDiagnosticsParams _ _ (LSP.List diagnostics)) -> return diagnostics
      Nothing -> fail "Expected diagnostics but received none"

waitForAllDiagnostics :: HashSet.HashSet LSP.Uri -> LSPM ()
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

bgroupIf :: Bool -> String -> [Criterion.Benchmark] -> Criterion.Benchmark
bgroupIf condition groupName benchmarks =
  Criterion.bgroup groupName $
  if condition
    then benchmarks
    else []

filterBenchmarks :: (String -> Bool) -> Criterion.Benchmark -> Criterion.Benchmark
filterBenchmarks predicate = fromJust . go []
  where
    go :: [String] -> Criterion.Benchmark -> Maybe Criterion.Benchmark
    go nameStack =
      \case
        Criterion.BenchGroup groupName benchmarks -> Just $ Criterion.BenchGroup groupName benchmarks'
          where benchmarks' = Data.Maybe.mapMaybe (go (nameStack ++ [groupName])) benchmarks
        Criterion.Benchmark benchmarkName _
          | not (predicate fullBenchmarkName) -> Nothing
          where fullBenchmarkName = foldl Criterion.addPrefix "" (nameStack ++ [benchmarkName])
        other -> Just other
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
