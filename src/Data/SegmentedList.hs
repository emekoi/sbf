{-# LANGUAGE TypeFamilies #-}

module Data.SegmentedList
  ( List (..),
    empty,
    null,
    modify,
    (!),
    (!?),
  )
where

import Data.Bits (shiftL)
import Data.Default
import Data.Int (Int64)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Prelude hiding (length, null)
import qualified Prelude as P

data List a
  = List
      { length :: Int64,
        capacity :: Int64,
        shelf :: [Vector a]
      }

instance Show a => Show (List a) where
  show = show . V.toList . V.concat . shelf

instance Functor List where
  fmap f l = l {shelf = (map . V.map) f (shelf l)}

empty :: List a
empty =
  List
    { length = 0,
      capacity = 0,
      shelf = []
    }

null :: List a -> Bool
null l = (length l) == 0

-- partial
(!) :: Default a => List a -> Int64 -> a
l ! i = ((shelf l) !! (fromIntegral si)) V.! bi
  where
    si = shelfIndex i
    bi = fromIntegral $ boxIndex i si

-- partial
(!?) :: Default a => List a -> Int64 -> a
l !? i = (grow l i) ! i
{-# INLINE (!?) #-}

-- partial
modify :: Default a => List a -> Int64 -> (a -> a) -> List a
modify l i f =
  let (ys, zs) = splitAt si (shelf l)
   in l {shelf = ys ++ (nv : tail zs)}
  where
    si = fromIntegral $ shelfIndex i
    nv = V.modify (\v -> M.write v (fromIntegral i) (f (l !? i))) (shelf l !! si)

grow :: Default a => List a -> Int64 -> List a
grow l sz
  | sc' > sc =
    let f i a =
          if i == sc
            then (V.replicate (fromIntegral $ shelfSize sc) def) : a
            else (V.replicate (fromIntegral $ shelfSize i) def) : f (i - 1) a
     in l
          { shelf = (shelf l) ++ f sc' [],
            capacity = (capacity l) + dc
          }
  | otherwise = l
  where
    sc' = shelfCount sz
    sc = fromIntegral $ P.length (shelf l)
    dc = foldr1 ((+) . shelfSize) [sc .. sc']

log2 :: Integral a => a -> a
log2 = truncate . logBase (2 :: Double) . fromIntegral

log2Ceil :: Integral a => a -> a
log2Ceil = ceiling . logBase (2 :: Double) . fromIntegral

shelfCount :: Int64 -> Int64
shelfCount = log2Ceil . (+ 1)

shelfSize :: Int64 -> Int64
shelfSize = (1 `shiftL`) . fromIntegral . log2 . (1 +)

shelfIndex :: Int64 -> Int64
shelfIndex = log2 . (+ 1)

boxIndex :: Int64 -> Int64 -> Int64
boxIndex a b = (a + 1) - (1 `shiftL` (fromIntegral b))
