{-# LANGUAGE ExistentialQuantification #-}

module SBF.Compiler.Pass
  ( Pass (..),
    CompilerPass (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import SBF.Parser (OpProgram)

data CompilerPass = forall a. Pass a => CompilerPass a

-- import SBF.Compiler.Pass.ClearZero (ClearZero (..))
-- import SBF.Compiler.Pass.RLE (RLE (..))

-- data CompilerPass where
--   CompilerPass :: Pass a => a -> CompilerPass

class Pass a where
  apply :: MonadIO m => a -> OpProgram -> m OpProgram

instance Pass CompilerPass where
  apply (CompilerPass a) = apply a
