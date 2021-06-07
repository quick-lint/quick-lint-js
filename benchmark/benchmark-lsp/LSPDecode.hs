-- Code in this file is derivative of lsp-test.
--
-- Copyright 2021 Matthew "strager" Glazar
-- Copyright Luke Lau 2018-2020.
-- All rights reserved.
-- See end of file for extended copyright information.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module LSPDecode where

import qualified Control.Exception as Exception
import Control.Lens ((^.))
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable (foldl')
import qualified Data.Functor.Product as Product
import qualified Data.IxMap as IxMap
import qualified Data.Kind as Kind
import Data.Maybe (fromJust)
import qualified Language.LSP.Test as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified System.IO as IO
import qualified System.IO.Error as IOError

-- | Fetches the next message bytes based on
-- the Content-Length header
getNextMessage :: IO.Handle -> IO BS.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing -> Exception.throw LSP.NoContentLengthHeader
    Just size -> BS.hGet h size

addHeader :: BS.ByteString -> BS.ByteString
addHeader content = BS.concat ["Content-Length: ", BS.pack $ show $ BS.length content, "\r\n", "\r\n", content]

getHeaders :: IO.Handle -> IO [(String, String)]
getHeaders h = do
  l <- Exception.catch (IO.hGetLine h) eofHandler
  let (name, val) = span (/= ':') l
  if null val
    then return []
    else ((name, drop 2 val) :) <$> getHeaders h
  where
    eofHandler e
      | IOError.isEOFError e = Exception.throw LSP.UnexpectedServerTermination
      | otherwise = Exception.throw e

type RequestMap = IxMap.IxMap LSP.LspId (LSP.SMethod :: LSP.Method 'LSP.FromClient 'LSP.Request -> Kind.Type)

newRequestMap :: RequestMap
newRequestMap = IxMap.emptyIxMap

updateRequestMap :: RequestMap -> LSP.LspId m -> LSP.SClientMethod m -> Maybe RequestMap
updateRequestMap reqMap messageID method = IxMap.insertIxMap messageID method reqMap

getRequestMap :: [LSP.FromClientMessage] -> RequestMap
getRequestMap = foldl' helper IxMap.emptyIxMap
  where
    helper :: RequestMap -> LSP.FromClientMessage -> RequestMap
    helper acc message =
      case message of
        LSP.FromClientMess m mess ->
          case LSP.splitClientMethod m of
            LSP.IsClientNot -> acc
            LSP.IsClientReq -> fromJust $ updateRequestMap acc (mess ^. LSP.id) m
            LSP.IsClientEither ->
              case mess of
                LSP.NotMess _ -> acc
                LSP.ReqMess msg -> fromJust $ updateRequestMap acc (msg ^. LSP.id) m
        _ -> acc

decodeFromServerMsg :: RequestMap -> BS.ByteString -> (RequestMap, LSP.FromServerMessage)
decodeFromServerMsg reqMap bytes = unP $ Aeson.parse p obj
  where
    obj = fromJust $ Aeson.decode bytes :: Aeson.Value
    p =
      LSP.parseServerMessage $ \lid ->
        let (mm, newMap) = IxMap.pickFromIxMap lid reqMap
         in case mm of
              Nothing -> Nothing
              Just m -> Just $ (m, Product.Pair m (Lens.Const newMap))
    unP (Aeson.Success (LSP.FromServerMess m msg)) = (reqMap, LSP.FromServerMess m msg)
    unP (Aeson.Success (LSP.FromServerRsp (Product.Pair m (Lens.Const newMap)) msg)) = (newMap, LSP.FromServerRsp m msg)
    unP (Aeson.Error e) = error e
-- Copyright 2021 Matthew "strager" Glazar
-- Copyright Luke Lau 2018-2020.
-- All rights reserved.
--
-- This file is part of quick-lint-js.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Luke Lau nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
