{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graphviz
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (partition)
import Pipes
import qualified Pipes.Prelude as P
import qualified KinGlow.Individual as I
import qualified KinGlow.Family as F
import qualified KinGlow.Fgl.Family as F
import qualified KinGlow.Fgl.Individual as I
import KinGlow.Fgl.Types
import KinGlow.Pipes
import System.IO
import Text.Gedcom.Types

main :: IO ()
main = do
    (nodes, gnodes) <- withFile "../gedcom/test2.ged" ReadMode f
    gfull <- withFile "../gedcom/test2.ged" ReadMode (g nodes gnodes)
    putStr . graphviz' $ gfull
  where
    -- e = return (empty :: KinGraph Gr)
    e = (empty :: KinGraph Gr)
--    f h = P.foldM inslog e return (gedcom h >-> P.filter I.isIndi)
    f h = P.foldM insTrack (return (HS.empty,e)) return (gedcom h >-> P.filter I.isIndi)
    g hs gr h = P.foldM inslog (return (hs, gr)) (return . snd) (gedcom h >-> P.filter F.isFam)

    insTrack (hs,gr) record = maybe (print record >> return (hs,gr)) (\(x,_) -> return (HS.insert x hs, gr')) node
      where
        node = I.indiNode record
        gr' = I.insert gr record

    inslog (hs, gr) record = do
        let edges = F.famEdges record
            (accepts, rejects) = check hs edges
        unless (null rejects) (print rejects)
        return $ (hs, F.insert gr record)


check :: HashSet Node -> [LEdge RelType] -> ([LEdge RelType], [LEdge RelType])
check hs = partition f
  where 
    f (x,y,_) = HS.member x hs && HS.member y hs


printRec :: Gedcom -> IO Gedcom
-- printRec r@(Gedcom (XRef x) _ _) | x == "I1037" = print "found" >> return r
printRec r@(Gedcom (XRef x) _ _) = C8.putStrLn x >> return r
-- printRec r = return r

