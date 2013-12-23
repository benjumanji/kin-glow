{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graphviz
import Data.GraphViz hiding (empty)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing hiding (empty)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Pipes
import qualified Pipes.Prelude as P
import qualified KinGlow.Individual as IP
import qualified KinGlow.Family as FP
import qualified KinGlow.Fgl.Family as F
import qualified KinGlow.Fgl.Individual as I
import KinGlow.Fgl.Types
import KinGlow.Pipes
import System.IO
import Text.Gedcom.Types hiding (name)

-- #ff7f0e
d3Orange :: Color
d3Orange = RGB 255 127 14

gtgParams = nonClusteredParams 
    { isDirected = True
    , globalAttributes = [GraphAttrs [Overlap (PrismOverlap Nothing), OutputOrder EdgesFirst, Layout Sfdp]]
    } 

gvParams = gtgParams 
    { fmtNode = \(_, x) -> case x of
                          Indi name -> [textLabel (T.fromStrict name), shape DoubleCircle]
                          Marr -> [textLabel "Family", shape Pentagon]
    , fmtEdge = \(_, _, x) -> case x of
                                  Married -> [textLabel "Married"]  
                                  ChildOf -> [textLabel "Child of"]  
    }

main :: IO ()
main = do
    gr <- graphFromFile
    withPosition <- graphToGraph gtgParams gr
    return ()
    -- T.putStr . printDotGraph . graphToDot gvParams $ gr

graphFromFile :: IO (KinGraph Gr)
graphFromFile = evalKinGlowT $ do 
    h <- liftIO $ openFile "../gedcom/test2.ged" ReadMode
    gnodes <- f h
    liftIO $ hSeek h AbsoluteSeek 0
    gfull <- g gnodes h
    liftIO $ hClose h
    return gfull
  where
    e = (empty :: KinGraph Gr)
    f h = P.foldM I.update (return e) return (gedcom h >-> P.filter IP.isIndi)
    g gr h = P.foldM F.update (return gr) return (gedcom h >-> P.filter FP.isFam)
