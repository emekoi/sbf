module SBF.Parser
  ( OpProgram (..),
    JumpTable,
    Op (..),
    Int64,
    Word8,
    parse,
    computeJmpTable,
  )
where

-- import Data.Monoid (Monoid (..))

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashTable.IO as H
import Data.Int (Int64)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

data Op
  = Inc Int64
  | Dec Int64
  | IDp Int64
  | DDp Int64
  | Out
  | In
  | Jz
  | Jnz
  | Clz
  | Nop
  deriving (Show)

instance Eq Op where
  (Inc _) == (Inc _) = True
  (Dec _) == (Dec _) = True
  (IDp _) == (IDp _) = True
  (DDp _) == (DDp _) = True
  Jnz == Jnz = True
  Jz == Jz = True
  Out == Out = True
  In == In = True
  Nop == Nop = True
  _ == _ = False

instance Semigroup Op where
  Nop <> a = a
  a <> Nop = a
  (Inc a) <> (Inc b) = Inc (a + b)
  (Dec a) <> (Dec b) = Dec (a + b)
  (IDp a) <> (IDp b) = IDp (a + b)
  (DDp a) <> (DDp b) = DDp (a + b)
  a <> b = error $ "invalid invocation: " ++ show a ++ " <> " ++ show b

instance Monoid Op where
  mempty = Nop

type JumpTable = H.CuckooHashTable Int64 Int64

data OpProgram
  = OpProgram
      { jmpTable :: JumpTable,
        program :: Vector Op
      }
  deriving (Show)

parse :: (HasCallStack, MonadIO m) => B.ByteString -> m OpProgram
parse i = liftIO $ do
  jt <- H.newSized 0
  v <- V.unfoldrM (parse' (B.filter (`B.elem` "[-><+,.]") i) jt) (0, [])
  return $
    OpProgram
      { jmpTable = jt,
        program = v
      }

safeSplit :: HasCallStack => [Int64] -> (Int64, [Int64])
safeSplit [] = error "unmatched '[' and ']'"
safeSplit (x : xs) = (x, xs)
{-# INLINE safeSplit #-}

parse' :: (HasCallStack, MonadIO m) => B.ByteString -> JumpTable -> (Int64, [Int64]) -> m (Maybe (Op, (Int64, [Int64])))
parse' s jt (i, js) =
  if i >= B.length s
    then return Nothing
    else case B.index s i of
      '-' -> rJust (Dec 1, (i + 1, js))
      '+' -> rJust (Inc 1, (i + 1, js))
      '>' -> rJust (IDp 1, (i + 1, js))
      '<' -> rJust (DDp 1, (i + 1, js))
      '.' -> rJust (Out, (i + 1, js))
      ',' -> rJust (In, (i + 1, js))
      '[' -> rJust (Jz, (i + 1, i : js))
      ']' -> liftIO $ do
        let (t, t') = safeSplit js
        H.insert jt i t
        H.insert jt t i
        return . Just $ (Jnz, (i + 1, t'))
      _ -> rJust (Nop, (i + 1, js))
  where
    rJust = return . Just
{-# INLINE parse' #-}

computeJmpTable :: MonadIO m => Vector Op -> m JumpTable
computeJmpTable v = liftIO $ do
  jt <- H.newSized 0
  V.foldM_ comp (0, [], jt) v
  return jt
  where
    comp (i, js, jt) o = do
      case o of
        Jz -> return (i + 1, i : js, jt)
        Jnz -> do
          let (t, r) = safeSplit js
          H.insert jt i t
          H.insert jt t i
          return (i + 1, r, jt)
        _ -> return (i + 1, js, jt)
