{-# LANGUAGE OverloadedStrings #-}

module KinGlow.Pipes
  (gedcom)
  where
      
import Control.Applicative
import Data.ByteString (ByteString)
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Attoparsec (parseMany)
import qualified Pipes.ByteString as PB hiding (ByteString)
import System.IO (Handle)
import Text.Gedcom.Types
import qualified Text.Gedcom.Parser as Parser

gedcom :: Handle -> Producer Gedcom IO ()
gedcom = gedcomPipe . PB.fromHandle 

gedcomPipe :: Producer ByteString IO () -> Producer Gedcom IO ()
gedcomPipe p = () <$ parseMany Parser.gedcom p >-> P.map snd 
