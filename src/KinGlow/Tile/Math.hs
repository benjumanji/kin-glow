{-# LANGUAGE TupleSections, BangPatterns #-}

module KinGlow.Tile.Math
  where

import Data.Hashable
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.Ord

data Point = P {-# UNPACK #-} !Int 
               {-# UNPACK #-} !Int
               deriving (Eq, Show)

instance Ord Point where
    (P x1 y1) `compare` (P x2 y2) = 
        case (x1 `compare` x2) of
            EQ -> y1 `compare` y2
            x  -> x

instance Hashable Point where
    hashWithSalt s (P x y) = s `hashWithSalt` x `hashWithSalt` y

-- | tile with coords from bottom left clockwise
data Tile = Tile Point Point Point Point deriving (Eq, Show)

instance Hashable Tile where
    hashWithSalt s (Tile (P x1 y1) (P x2 y2) (P x3 y3) (P x4 y4)) = h3
      where
        !h1 = s `hashWithSalt` x1 `hashWithSalt` y1 `hashWithSalt` x2 `hashWithSalt` y2
        !h2 = x3 `hashWithSalt` y3 `hashWithSalt` x4 `hashWithSalt` y4
        !h3 = h1 `hashWithSalt` h2


-- | smart constructor: given a size and lower left coord return the tile
tile1 :: Point -- ^ lower left corner
      -> Int   -- ^ size
      -> Tile  -- tile
tile1 p s = tile2 p s s
{-# INLINE tile1 #-}

-- | smart constructor: given a lower left coordinate, height and width
--   return the tile
tile2 :: Point -- ^ lower left corner
      -> Int   -- ^ width
      -> Int   -- ^ height
      -> Tile  -- ^ tile
tile2 p@(P x y) sx sy =
    let x' = x + sx
        y' = y + sy
    in Tile p (P x y') (P x' y') (P x' y)

-- | generate a list of tiles from an rectangle
tiles :: Int -- ^ width of rect 
      -> Int -- ^ height of rect
      -> Int -- ^ tile size
      -> Int -- ^ tile overlap
      -> [Tile]
tiles w h s o =
    let (nh, remh) = h `quotRem` s
        (nw, remw) = w `quotRem` s
        first = do
            x <- [0..(nw-1)]
            y <- [0..(nh-1)]
            return $ tile1 (P (x * s) (y * s)) s
        lasth = if remh == 0 
                    then [] 
                    else map (\x -> (tile2 (P (x * s) (nh * s)) s remh)) [0..(nw-1)]
        lastw = if remw == 0
                    then []
                    else map (\y -> (tile2 (P (nw * s) (y * s)) remw s)) [0..(nh-1)]
        last  = if remh == 0 || remw == 0 
                    then [] 
                    else [tile2 (P (nw * s) (nh * s)) remw remh]
    in (first ++ (lasth ++ (lastw ++ last)))

-- | given a bounding box, list of points and a tile size, associate tiles to 
--   points
assocTiles :: Int                  -- ^ width
           -> Int                  -- ^ height
           -> Int                  -- ^ tile size
           -> [Point]              -- ^ list of points
           -> HashMap Tile [Point] -- ^ tile map 
assocTiles w h s = foldr f tileMap
  where
    tileMap = HM.fromList . map (,[]) $ tiles w h s 0
    f p tm = HM.insert t (p:(tm ! t)) tm
      where
        t = getTile w h s p

-- | given a bounding box and a point, get it's tile
getTile w h s (P x y) = tile2 (P (nw * s) (nh * s)) sx sy
  where
    (!nw, !remw) = x `quotRem` s
    (!nh, !remh) = y `quotRem` s
    !sx = if w >= (nw + 1) * s then s else remw
    !sy = if w >= (nh + 1) * s then s else remh


