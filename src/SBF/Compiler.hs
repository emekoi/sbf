{-# LANGUAGE RankNTypes #-}

module SBF.Compiler
  ( CompileOpts (..),
    Program (..),
    Pass (..),
    compile,
  )
where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L
import GHC.Stack (HasCallStack)
import SBF.Compiler.Pass (Pass (..))
import qualified SBF.Interpreter as I
import qualified SBF.Parser as P

data CompileOpts
  = CompileOpts
      { inFile :: String,
        outFile :: String,
        interpret :: Bool
      }
  deriving (Eq, Show)

data Program
  = Interpreted I.Interpreter
  | Compiled
  deriving (Show)

compile :: (HasCallStack, MonadIO m, Pass p) => CompileOpts -> [p] -> L.ByteString -> m Program
compile o p i = do
  p' <- flip (foldM (flip apply)) p =<< P.parse i
  return $
    if interpret o
      then Interpreted (I.interpret p')
      else Compiled
