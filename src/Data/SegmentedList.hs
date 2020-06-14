{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SegmentedList
  ( SList (..),
    empty,
    null,
    modify,
    modify',
    modifyGrow,
    grow,
    (!),
    (!#),
  )
where

import Data.Bits (shiftL)
import Data.Default
import Data.Int (Int64)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null)
import qualified Prelude as P

newtype VList a = VList [Vector a]

data SList a
  = SList
      { length :: Int64,
        capacity :: Int64,
        _shelf :: VList a
      }
  deriving (Show)

shelf :: SList a -> [Vector a]
shelf (SList _ _ (VList a)) = a

instance Show a => Show (VList a) where
  show (VList a) = show . V.toList . V.concat $ a

instance Functor SList where
  fmap f l = l {_shelf = VList $ (map . V.map) f (shelf l)}

empty :: SList a
empty =
  SList
    { length = 0,
      capacity = 0,
      _shelf = VList []
    }

null :: SList a -> Bool
null l = length l == 0

(!) :: (HasCallStack, Default a) => SList a -> Int64 -> a
l ! i = (shelf l !! fromIntegral si) V.! bi
  where
    si = shelfIndex i
    bi = fromIntegral $ boxIndex i si

(!#) :: (HasCallStack, Default a) => SList a -> Int64 -> a
l !# i = grow l (i + 1) ! i
{-# INLINE (!#) #-}

modify :: (HasCallStack, Default a) => SList a -> Int64 -> (a -> a) -> SList a
modify l i f =
  let (ys, zs) = splitAt (fromIntegral si) (shelf l)
      (dv, zs') = splitAt 1 zs
   in l {_shelf = VList $ ys ++ (nv (head dv) : zs')}
  where
    si = shelfIndex i
    bi = fromIntegral $ boxIndex i si
    nv v = V.modify (\v -> M.modify v f bi) v
{-# INLINE modify #-}

modify' :: (HasCallStack, Default a) => SList a -> Int64 -> (a -> a) -> SList a
modify' l i f =
  if i >= capacity l
    then error "index out of bounds"
    else modify l i f
{-# INLINE modify' #-}

modifyGrow :: (HasCallStack, Default a) => SList a -> Int64 -> (a -> a) -> SList a
modifyGrow l i = modify (grow l (i + 1)) i
{-# INLINE modifyGrow #-}

grow :: (HasCallStack, Default a) => SList a -> Int64 -> SList a
grow l sz
  | sc' > sc =
    let f i a =
          if i == sc'
            then a
            else V.replicate (fromIntegral $ shelfSize i) def : f (i + 1) a
     in l
          { _shelf = VList $ shelf l ++ f sc [],
            capacity = capacity l + dc
          }
  | otherwise = l
  where
    sc' = shelfCount sz
    sc = fromIntegral $ P.length (shelf l)
    dc = sum $ shelfSize <$> [sc .. (sc' - 1)]
{-# INLINE grow #-}

log2 :: Integral a => a -> a
log2 = truncate . logBase (2 :: Double) . fromIntegral
{-# INLINE log2 #-}

log2Ceil :: Integral a => a -> a
log2Ceil = ceiling . logBase (2 :: Double) . fromIntegral
{-# INLINE log2Ceil #-}

shelfCount :: Int64 -> Int64
shelfCount = log2Ceil . (+ 1)
{-# INLINE shelfCount #-}

shelfSize :: Int64 -> Int64
shelfSize = (1 `shiftL`) . fromIntegral
{-# INLINE shelfSize #-}

shelfIndex :: Int64 -> Int64
shelfIndex = log2 . (+ 1)
{-# INLINE shelfIndex #-}

boxIndex :: Int64 -> Int64 -> Int64
boxIndex i s = (i + 1) - (1 `shiftL` fromIntegral s)
{-# INLINE boxIndex #-}
