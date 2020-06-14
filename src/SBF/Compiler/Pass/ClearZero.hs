module SBF.Compiler.Pass.ClearZero
  ( ClearZero (..),
  )
where

import qualified Data.List as L
import qualified Data.Vector as V
import GHC.Stack (HasCallStack)
import SBF.Compiler.Pass (Pass (..))
import SBF.Parser (Op (..), OpProgram (..), computeJmpTable)

data ClearZero = ClearZero

zeroMinus :: [Op]
zeroMinus = [Jz, Dec (-1), Jnz]

zeroPlus :: [Op]
zeroPlus = [Jz, Inc (-1), Jnz]

-- goes `from` -> `to` in x
replace :: (HasCallStack, Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just xs <- L.stripPrefix from xs = to ++ replace from to xs
replace from to (x : xs) = x : replace from to xs
replace _ _ [] = []

instance Pass ClearZero where
  apply _ (OpProgram _ p) = do
    let p' = V.fromList . replace zeroMinus [Clz] . replace zeroPlus [Clz] . V.toList $ p
    jt <- computeJmpTable p'
    return
      OpProgram
        { program = p',
          jmpTable = jt
        }
