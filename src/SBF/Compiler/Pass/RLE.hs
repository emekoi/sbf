module SBF.Compiler.Pass.RLE
  ( RLE (..),
  )
where

import qualified Data.List as L
import qualified Data.Vector as V
import SBF.Compiler.Pass (Pass (..))
import SBF.Parser (Op (..), OpProgram (..), computeJmpTable)

data RLE = RLE

instance Pass RLE where
  apply _ (OpProgram _ p) = do
    let p' = V.fromList . map mconcat . L.groupBy gf . V.toList $ p
    jt <- computeJmpTable p'
    return
      OpProgram
        { jmpTable = jt,
          program = p'
        }
    where
      gf' o = any ($ o) [(== In), (== Out), (== Jz), (== Jnz)]
      gf a b = case (gf' a, gf' b) of
        (True, True) -> False
        (True, False) -> False
        (False, True) -> False
        (_, _) -> a == b
