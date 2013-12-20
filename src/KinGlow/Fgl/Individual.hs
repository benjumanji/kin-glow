{-# LANGUAGE TupleSections #-}

module KinGlow.Fgl.Individual
  (insert, indiNode)
  where

import Control.Applicative
import qualified Data.ByteString.Char8 as C8
import Data.Graph.Inductive
import Text.Gedcom.Types
import KinGlow.Fgl.Types


insert :: DynGraph gr => KinGraph gr -> Gedcom -> KinGraph gr
insert gr record = maybe gr f indi
  where
    f = flip insNode gr
    indi = indiNode record

indiNode :: Gedcom -> Maybe UNode 
indiNode (Gedcom (XRef xref) _ _) = (,()) <$> ref
  where
    ref = fst <$> C8.readInt (C8.tail xref)
