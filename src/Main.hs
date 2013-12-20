{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main 
  where

import Blaze.ByteString.Builder.ByteString as B (fromLazyByteString)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 hiding (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Text()
import Network.Http.Client
import Pipes
import System.IO (withFile, IOMode(..))
import System.IO.Streams (stdout)
import qualified System.IO.Streams as Streams

import qualified KinGlow.Individual as I
import qualified KinGlow.Family as F
import KinGlow.Pipes
import Text.Gedcom.Types

main :: IO ()
main = withFile "../gedcom/test2.ged" ReadMode (runEffect . g) 
  where
    g h = gedcom h >-> insertNodesForTag

insertNodesForTag :: Consumer Gedcom IO ()
insertNodesForTag = forever insert
  where
    insert = await >>= lift . mapM_ insertQuery . gen
--    gen x | I.isIndi x = I.create x
    gen x | F.isFam  x = F.create x
          | True       = []

insertQuery :: L.ByteString -> IO ()
insertQuery bs = do
    c <- openConnection "127.0.0.1" 7474
    L.putStrLn bs
    q <- buildRequest $ do
        http POST "/db/data/cypher"
        setAccept "application/json"
        setContentType "application/json"
    sendRequest c q (\o -> Streams.write (Just (B.fromLazyByteString bs)) o)
    receiveResponse c (\_ i -> Streams.connect i stdout)
    closeConnection c
