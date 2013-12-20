{-# LANGUAGE TupleSections #-}

module KinGlow.Fgl.Family
  (insert, famEdges)
  where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Graph.Inductive
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Text.Gedcom.Types
import KinGlow.Fgl.Types


insert :: DynGraph gr => KinGraph gr -> Gedcom -> KinGraph gr
insert gr record = insEdges (famEdges record) gr

famEdges :: Gedcom -> [LEdge RelType]
famEdges (Gedcom _ _ tree) = fedges
  where
    tree' = rewriteTyped tree
    getRef g = (fst <$>) . ((C8.readInt . C8.tail) <=< g)
    w = getRef wife tree'
    h = getRef husb tree'
    cs = mapMaybe ((fst <$>) . C8.readInt . C8.tail) (children tree')
    marriagee = marriageEdge <$> h <*> w
    childhes = fromMaybe [] $ childEdges <$> h <*> pure cs 
    childwes = fromMaybe [] $ childEdges <$> w <*> pure cs
    childes = childhes ++ childwes
    fedges = maybe childes (:childes) marriagee

marriageEdge :: Int -> Int -> LEdge RelType
marriageEdge i1 i2 = (i1,i2,Married)

childEdges :: Int -> [Int] -> [LEdge RelType]
childEdges p = map (,p,ChildOf)
