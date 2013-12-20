{-# LANGUAGE TupleSections, OverloadedStrings #-}

module KinGlow.Types
  where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

data Query a = Query
    { _query :: ByteString
    , _params :: a
    }

data PropsQuery a = PropsQuery ByteString a

instance ToJSON a => ToJSON (Query a) where
    toJSON (Query q p) = object ["query" .= q, "params" .= (toJSON p)]

instance ToJSON a => ToJSON (PropsQuery a) where
    toJSON (PropsQuery q p) = object ["query" .= q, "params" .= f p]
      where 
        f = toJSON . HM.singleton ("props" :: Text)

