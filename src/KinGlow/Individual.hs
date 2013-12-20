{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module KinGlow.Individual
  ( create
  , isIndi
  )
  where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Text()
import Data.Generics.Uniplate.Operations
import Text.Gedcom.Types

import KinGlow.Types

create :: Gedcom -> [L.ByteString]
create (Gedcom (XRef xref) _ trees) = queries
  where
    trees' = rewriteTyped trees
    hm = HM.fromList $ ("xref", xref):pairs
    pairs = do
        (tagValue -> Just (t,v)) <- childrenBi trees'
        guard (t == "SEX")
        return (t,v)
    nameq =  (map encode . nameQuery hm) <$> name trees'
    deathq = (map encode . deathQuery xref) <$> death trees'
    birthq = (map encode . birthQuery xref) <$> birth trees'
    queries = concat . catMaybes $ [nameq, deathq, birthq]

nameQuery :: HashMap ByteString ByteString
          -> Name
          -> [PropsQuery (HashMap ByteString ByteString)]
nameQuery hm (Name rw g s) = [PropsQuery text hm']
  where
    text = "create (i:INDI {props})"
    hm' = f hm
    f = HM.insert "raw-name" rw . ins "given" g . ins "surname" s
    ins key (Just v) map = HM.insert key v map
    ins _   _        map = map

-- | construct a query that will create a relationship indicating place of
--   death. The query will create a new place node only if no existing
--   place is matched.
deathQuery :: ByteString        -- ^ individual xref
           -> Death             -- ^ place of death 
           -> [Query (HashMap ByteString ByteString)]
deathQuery xref (Death place _) = [pquery, rquery] 
  where
    ptext = "merge (p:PLACE {name:{NAME}})"
    pparams = HM.singleton "NAME" place
    pquery = Query ptext pparams
    rtext = "match (p:PLACE {name:{NAME}}), (i:INDI {xref:{XREF}}) create unique (i)-[:DIEDIN]-(p)"
    rparams = HM.fromList [("NAME", place), ("XREF", xref)]
    rquery = Query rtext rparams

-- | construct a query that will create a relationship indicating place of
--   birth. The query will create a new place node only if new existing
--   place is matched.
birthQuery :: ByteString
           -> Birth
           -> [Query (HashMap ByteString ByteString)]
birthQuery xref (Birth place _) = [pquery, rquery]
  where
    ptext = "merge (p:PLACE {name:{NAME}})"
    pparams = HM.singleton "NAME" place
    pquery = Query ptext pparams
    rtext = "match (p:PLACE {name:{NAME}}), (i:INDI {xref:{XREF}}) create unique (i)-[:BORNIN]-(p)"
    rparams = HM.fromList [("NAME", place), ("XREF", xref)]
    rquery = Query rtext rparams

data PlaceParams = PP ByteString ByteString

instance ToJSON PlaceParams where
    toJSON (PP xref place) = object ["xprops" .= xobj, "pprops" .= pobj]
      where
        xobj = object ["xref" .= (toJSON xref)]
        pobj = object ["name" .= (toJSON place)]

isIndi :: Gedcom -> Bool
isIndi (Gedcom _ tag _) = tag == "INDI"
