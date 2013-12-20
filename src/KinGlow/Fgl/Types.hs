module KinGlow.Fgl.Types
  where

type KinGraph gr = gr () RelType

data RelType = 
      Married
    | ChildOf 
    deriving (Eq, Show)


