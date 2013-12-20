{-# LANGUAGE OverloadedStrings #-}

module Main 
  where

import Blaze.ByteString.Builder as B 
import Blaze.ByteString.Builder.ByteString as B 
import Control.Lens hiding ((.=))
import Control.Lens.Aeson
import Data.Aeson
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Monoid
import Network.Http.Client
import Network.HTTP.Types (renderSimpleQuery)
import qualified System.IO.Streams as S

main :: IO ()
main = do
    let body = encode $ EmptyQuery "match (p:PLACE) return p limit 10"
    res <- readQuery body
    let x = map (\(String x) -> x) (res ^.. key "data" . _Array . traverse . nth 0 . key "data" . key "name")
    mapM_ (geoCodeRequest . T.encodeUtf8) x
    C8.putStrLn "done"
       
readQuery :: L.ByteString -> IO L.ByteString
readQuery bs = do
    c <- openConnection "127.0.0.1" 7474
    q <- buildRequest $ do 
            http POST "/db/data/cypher"
            setAccept "application/json"
            setContentType "application/json"
    sendRequest c q (\o -> S.write (Just (B.fromLazyByteString bs)) o)
    res <- receiveResponse c (\s i -> do
                                i2 <- S.map B.fromByteString i
                                x <- S.fold mappend mempty i2
                                return $ B.toLazyByteString x)
    closeConnection c
    return res

params :: ByteString -> ByteString
params x = renderSimpleQuery True ps
  where
    ps = [ ("format", "json")
         , ("source", "OSM")
         , ("q", x)
         ]

geoCodeRequest :: ByteString -> IO ()
geoCodeRequest bs = do
    C8.putStrLn bs
    c <- openConnection "beta.geocoding.cloudmade.com" 80
    q <- buildRequest $ do
            http GET ("/v3/ba81157ae8bd49c688d0bf45b66cb464/api/geo.location.search.2" <> params bs)
            setAccept "application/json"
    sendRequest c q emptyBody
    receiveResponse c (const $ S.foldM (const C8.putStrLn) ())
    
data EmptyQuery = EmptyQuery ByteString

instance ToJSON EmptyQuery where
    toJSON (EmptyQuery x) = object ["query" .= toJSON x, "params" .= toJSON h]      where
        h = (HM.empty :: HashMap ByteString ByteString)

toThreePartName :: Text -> Maybe (Text,Text,Text)
toThreePartName = f . T.splitOn ","
  where
    f [x,y,z] = Just (x,y,z)
    f _       = Nothing
