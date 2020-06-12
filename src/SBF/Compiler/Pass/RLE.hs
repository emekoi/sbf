module SBF.Compiler.Pass.RLE
  ( RLE (..),
  )
where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import SBF.Compiler (Pass (..))
import SBF.Parser (Op (..))

newtype RLE = RLE ()

instance Pass RLE where
  apply _ o = V.modify (\v -> mapM_ (update v) jzl) $ V.fromList lo
    where
      update v (s, e) = do
        M.write v s (Jz . fromIntegral $ s)
        M.write v e (Jnz . fromIntegral $ s)
      lo = map mconcat . L.groupBy gf $ V.toList o
      jzl = zip (L.elemIndices (Jz (-1)) lo) (reverse $ L.elemIndices (Jnz (-1)) lo)
      gf a b = case a of
        In -> False
        Out -> False
        Jz _ -> False
        Jnz _ -> False
        _ -> a == b
