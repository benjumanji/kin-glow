{-# LANGUAGE OverloadedStrings, TupleSections #-}

module KinGlow.Family
  ( create
  , isFam
  )
  where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import Text.Gedcom.Types

import KinGlow.Types

create :: Gedcom -> [L.ByteString]
create (Gedcom _ _ tree) = map encode =<< catMaybes queries
  where
    tree' = rewriteTyped tree
    w = wife tree'
    h = husb tree'
    m = marriage tree'
    cs = children tree'
    marriageq = marriageQuery <$> h <*> w <*> m
    childhq = childrenQuery <$> h <*> pure cs 
    childwq = childrenQuery <$> w <*> pure cs 
    siblingq = Just $ siblingQuery cs
    queries = [marriageq, childhq, childwq, siblingq]


isFam :: Gedcom -> Bool
isFam (Gedcom _ t  _) | t == "FAM" = True
isFam _                            = False

marriageQuery :: ByteString -- ^ xref husb
              -> ByteString -- ^ xref wife
              -> Marriage   -- ^ marriage place
              -> [Query (HashMap ByteString ByteString)]
marriageQuery h w (Marriage place date) = [rquery, mquery, pquery]
  where
    rtext = "match (h {xref:{HREF}}), (w {xref:{WREF}}) create unique (h)-[:MARRIEDTO {date:{DATE}}]-(w)"
    rparams = HM.fromList [("HREF", h), ("WREF", w), ("DATE", date)]
    rquery = Query rtext rparams
    ptext = "merge (p:PLACE {name:{NAME}})"
    pparams = HM.singleton "NAME" place
    pquery = Query ptext pparams
    mtext = "match (h {xref:{HREF}}), (p:PLACE {name:{NAME}}) create (h)-[:MARRIEDIN {date:{DATE}}]->(p)<-[:MARRIEDIN {date:{DATE}}]-(w)"
    mparams = HM.fromList [("HREF", h), ("WREF", w), ("DATE", date), ("NAME", place)]
    mquery = Query mtext mparams

childrenQuery :: ByteString   -- parent xref
              -> [ByteString] -- child xrefs
              -> [Query (HashMap ByteString ByteString)]
childrenQuery p = map query 
  where
    text = "match (p {xref:{PREF}}), (c {xref:{CREF}}) create unique (p)<-[:CHILDOF]-(c)"
    parent = HM.singleton "PREF" p
    query c = Query text (HM.insert "CREF" c parent)

siblingQuery :: [ByteString]
             -> [Query (HashMap ByteString ByteString)]
siblingQuery cs = queries cs
  where
    f x y = HM.fromList [("XREF",x),("YREF",y)]
    pairs (x:xs) = (map (f x) xs) ++ pairs xs
    pairs [] = []
    text = "match (x {xref:{XREF}}),(y {xref:{YREF}}) create unique (x)-[:SIBLINGOF]-(y)"
    queries = map (\x -> Query text x) . pairs
