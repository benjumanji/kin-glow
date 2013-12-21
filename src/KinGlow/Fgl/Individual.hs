{-# LANGUAGE TupleSections, OverloadedStrings #-}

module KinGlow.Fgl.Individual
  (update)
  where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Graph.Inductive
import Data.Maybe (fromMaybe)
import Data.Text.Encoding
import Text.Gedcom.Types
import KinGlow.Fgl.Types


update :: (MonadIO m, Functor m, DynGraph gr) 
       => KinGraph gr              -- ^ geneology to update
       -> Gedcom                   -- ^ record to update
       -> KinGlowT m (KinGraph gr) -- ^ updated geneology
update gr (Gedcom (XRef xref) _ tree) = ((flip insNode gr) . (, Indi label)) <$> nodeId xref
  where
    tree' = rewriteTyped tree
    label = maybe "Anon." (\(Name x _ _) -> decodeUtf8 x) $ name tree'
    
