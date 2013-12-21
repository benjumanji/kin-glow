{-# LANGUAGE TupleSections, OverloadedStrings #-}

module KinGlow.Fgl.Family
  (update)
  where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Graph.Inductive.Graph
import Text.Gedcom.Types hiding (marriage)
import KinGlow.Fgl.Types


-- | update the graph with this families relationships
update :: (Monad m, Applicative m, DynGraph gr)
       => KinGraph gr              -- ^ the current geneology
       -> Gedcom                   -- ^ a record to add
       -> KinGlowT m (KinGraph gr) -- ^ updated geneology
update gr (Gedcom _ _ tree) = gr'
  where
    tree' = rewriteTyped tree
    mw = wife tree'
    mh = husb tree'
    cs = children tree'
    gr' = case (mw,mh) of
              (Just w, Just h) -> (& gr) <$> marriage h w cs
              (Just w, _)      -> (flip insEdges gr) <$> (singleParent w cs)
              (_, Just h)      -> (flip insEdges gr) <$> (singleParent h cs)
              _                -> pure gr

-- | create a marriage node and its context from the husbad wife and
--   children in the record
marriage :: (Monad m, Applicative m)
         => ByteString 
         -> ByteString
         -> [ByteString]
         -> KinGlowT m (Context NodeType RelType)
marriage h w cs = f <$> nodeId h <*> nodeId w <*> childids <*> getmid
  where
    getmid = nodeId $ "MARRIED|" <> h <> "|" <> w 
    childids = mapM nodeId cs
    f hid wid cids mid = (adjs, mid, Marr, [])
      where
        adjs = (Married,hid):(Married,wid):map (ChildOf,) cids
       
singleParent :: (Monad m, Applicative m)
             => ByteString                -- ^ single parent ref
             -> [ByteString]              -- ^ child refs
             -> KinGlowT m [LEdge RelType] -- ^ edge collection
singleParent p cs = f <$> nodeId p <*> mapM nodeId cs
  where
    f pid cids = map (,pid, ChildOf) cids
