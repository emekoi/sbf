module SBF.Interpreter
  ( Interpreter (..),
    interpret,
    run',
    run,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.HashTable.IO as H
import Data.SegmentedList ((!#), SList)
import qualified Data.SegmentedList as SL
import qualified Data.Vector as V
import Data.Vector ((!))
import GHC.Stack (HasCallStack)
import SBF.Parser (Int64, Op (..), OpProgram (..), Word8)

data Interpreter
  = Interpreter
      { tape :: SList Word8,
        code :: OpProgram,
        dp :: Int64,
        ip :: Int64
      }
  deriving (Show)

interpret :: OpProgram -> Interpreter
interpret p =
  Interpreter
    { tape = SL.empty,
      code = p,
      dp = 0,
      ip = 0
    }

run :: (HasCallStack, MonadIO m) => Interpreter -> m ()
run it = void (run' it)

run' :: (HasCallStack, MonadIO m) => Interpreter -> m Interpreter
run' it =
  if ip it >= fromIntegral (V.length . program . code $ it)
    then return it
    else
      let it' = step it $ program (code it) ! fromIntegral (ip it)
       in run' . (\i -> i {ip = ip i + 1}) =<< it'

step :: (HasCallStack, MonadIO m) => Interpreter -> Op -> m Interpreter
step it (Inc x) = return . mtape it $ (fromIntegral . (+ x) . fromIntegral)
step it (Dec x) = return . mtape it $ (fromIntegral . subtract x . fromIntegral)
step it (IDp x) = return $ it {dp = dp it + x}
step it (DDp x) = return $ it {dp = dp it - x}
step it Out = liftIO $ do
  putChar (w2c $ tape it !# dp it)
  return it
step it In = liftIO $ do
  i <- c2w <$> getChar
  return . mtape it $ const i
step it Jz = liftIO $ do
  case tape it !# dp it of
    0 -> do
      Just ip' <- H.lookup (jmpTable $ code it) (ip it)
      return it {ip = ip'}
    _ -> return it
step it Jnz = liftIO $ do
  case tape it !# dp it of
    0 -> return it
    _ -> do
      Just ip' <- H.lookup (jmpTable $ code it) (ip it)
      return it {ip = ip'}
step it Clz = return . mtape it $ const 0
step it Nop = return it

mtape :: HasCallStack => Interpreter -> (Word8 -> Word8) -> Interpreter
mtape it f =
  it
    { tape = SL.modifyGrow (tape it) (dp it) f
    }
