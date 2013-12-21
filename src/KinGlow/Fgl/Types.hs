module KinGlow.Fgl.Types
  ( KinGlowT
  , KinGlowState
  , KinGraph
  , NodeType(..)
  , RelType(..)
  , evalKinGlowT
  , nodeId
  , runKinGlowT
  , unsafeNodeId
  )
  where

import Control.Monad.Trans.State.Strict
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data KinGlowState = KGS
    { _fresh   :: Int
    , _nodeMap :: HashMap ByteString Int
    } deriving (Eq, Show)

type KinGlowT m a = StateT KinGlowState m a

type KinGraph gr = gr NodeType RelType

data NodeType = 
      Indi Text
    | Marr
    deriving (Eq, Show)

data RelType = 
      Married
    | ChildOf 
    deriving (Eq, Show)

runKinGlowT :: Monad m => KinGlowT m a -> m (a, HashMap ByteString Int)
runKinGlowT x = do 
    (x', kgs) <- runStateT x (KGS 0 HM.empty)
    return (x', kgs ^. nodeMap)

evalKinGlowT :: Monad m => KinGlowT m a -> m a
evalKinGlowT = flip evalStateT (KGS 0 HM.empty)

nodeMap :: Lens' KinGlowState (HashMap ByteString Int)
nodeMap f (KGS fr nm) = fmap (\nm' -> KGS fr nm') (f nm)

fresh :: Lens' KinGlowState Int
fresh f (KGS fr nm) = fmap (\fr' -> KGS fr' nm) (f fr)

-- | get a fresh id for the node if it doesn't exist, otherwise return the
--   registered node id
nodeId :: Monad m 
       => ByteString    -- ^ unique identifier for record
       -> KinGlowT m Int -- ^ unique node id
nodeId bs = do
    mid <- zoom nodeMap . gets $ HM.lookup bs
    maybe (unsafeNodeId bs) return mid

-- | if the node has already been registered this will overwrite the
--   existing node
unsafeNodeId :: Monad m => ByteString -> KinGlowT m Int
unsafeNodeId bs = do
    fr <- zoom fresh $ gets (+1)
    modify $ \(KGS _ nm) -> KGS fr (HM.insert bs fr nm)
    return fr
