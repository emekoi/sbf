{-# LANGUAGE DeriveAnyClass #-}

module SBF.Parser
  ( Op (..),
    Int64,
    Word8,
    parse,
  )
where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Int (Int64)
-- import Data.Monoid (Monoid (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)

data Op
  = Inc Int64
  | Dec Int64
  | IDp Int64
  | DDp Int64
  | Out
  | In
  | Jz Int64
  | Jnz Int64
  | Nop
  deriving (Show)

instance Eq Op where
  (Inc _) == (Inc _) = True
  (Dec _) == (Dec _) = True
  (IDp _) == (IDp _) = True
  (DDp _) == (DDp _) = True
  (Jz _) == (Jz _) = True
  (Jnz _) == (Jnz _) = True
  Out == Out = True
  In == In = True
  Nop == Nop = True
  _ == _ = False

-- our instance of Semigroup is partial but that's okay
instance Semigroup Op where
  Nop <> a = a
  a <> Nop = a
  (Inc a) <> (Inc b) = Inc (a + b)
  (Dec a) <> (Dec b) = Dec (a + b)
  (IDp a) <> (IDp b) = IDp (a + b)
  (DDp a) <> (DDp b) = DDp (a + b)

-- (Jz a) <> (Jz b) = Jz (a + b)
-- (Jnz a) <> (Jnz b) = Jnz (a + b)
-- Out <> Out = Out
-- In <> In = In

instance Monoid Op where
  mempty = Nop

parse :: L.ByteString -> Vector Op
parse i = V.unfoldr (parse' i) (0, [])

safeHead :: [Int64] -> Int64
safeHead [] = -1
safeHead (x : _) = x
{-# INLINE safeHead #-}

parse' :: L.ByteString -> (Int64, [Int64]) -> Maybe (Op, (Int64, [Int64]))
parse' l (i, t) =
  if i > L.length l
    then Nothing
    else Just $ case L.index l i of
      '+' -> (Inc 1, (i + 1, t))
      '-' -> (Dec 1, (i + 1, t))
      '>' -> (IDp 1, (i + 1, t))
      '<' -> (DDp 1, (i + 1, t))
      '.' -> (Out, (i + 1, t))
      ',' -> (In, (i + 1, t))
      '[' -> (Jz i, (i + 1, i : t))
      ']' -> (Jnz (safeHead t), (i + 1, tail t))
      _ -> (Nop, (i, t))
