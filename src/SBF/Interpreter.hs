module SBF.Interpreter
  ( Interpreter (..),
    interpret,
    run,
  )
where

-- import qualified Data.ByteString.Lazy as L

-- import qualified Data.Vector as V
-- import qualified SBF.Parser as P

import Data.SegmentedList (List)
import qualified Data.SegmentedList as SL
import Data.Vector (Vector)
import SBF.Parser (Op, Word8)

data Interpreter
  = Interpreter
      { tape :: List Word8,
        program :: Vector Op
      }

interpret :: Vector Op -> Interpreter
interpret p =
  Interpreter
    { tape = SL.empty,
      program = p
    }

run :: Interpreter -> IO ()
run _ = return ()
