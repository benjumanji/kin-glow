module KinGlow.DeepZoom
  where

import Data.Bits

data DeepZoom = DZ
    { _height :: Int
    , _width :: Int
    , _maxLevel :: Int
    } deriving (Eq, Show)

data BoundingBox = BB {-# UNPACK #-} !Int 
                      {-# UNPACK #-} !Int

deepZoomFromSpec :: BoundingBox -- ^ size of image in pts
                 -> Int         -- ^ size of node in pts
                 -> BoundingBox -- ^ size of monitor
                 -> DeepZoom
deepZoomFromSpec (BB x y) o _ = DZ dx dy (levels dx dy)
  where
    ratio = 256 `quot` o 
    dy = ratio * y
    dx = ratio * x

levels x y = levels' x y 0
{-# INLINE levels #-}

-- | calculate the number of levels
levels' :: Int -- ^ width
        -> Int -- ^ height
        -> Int -- ^ accumulator
        -> Int
levels' 1 1 l = l
levels' 1 y l = levels' 1 (div2c y) (l+1)
levels' x 1 l = levels' (div2c x) 1 (l+1)
levels' x y l = levels' (div2c x) (div2c y) (l+1)

div2c x = (x + 1) `shiftR` 1
{-# INLINE div2c #-}


