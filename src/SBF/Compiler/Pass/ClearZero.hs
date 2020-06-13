module SBF.Compiler.Pass.ClearZero
  ( ClearZero (..),
  )
where

import qualified Data.List as L
import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as M

import GHC.Stack (HasCallStack)
import SBF.Compiler (Pass (..))
import SBF.Parser (Op (..))

newtype ClearZero = ClearZero ()

zeroMinus :: [Op]
zeroMinus = [Jz (-1), Dec (-1), Jnz (-1)]

zeroPlus :: [Op]
zeroPlus = [Jz (-1), Inc (-1), Jnz (-1)]

-- goes `from` -> `to` in x
replace :: (HasCallStack, Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just xs <- L.stripPrefix from xs = to ++ replace from to xs
replace from to (x : xs) = x : replace from to xs
replace _ _ [] = []

instance Pass ClearZero where
  apply _ = V.fromList . replace zeroMinus [Clz] . replace zeroPlus [Clz] . V.toList
