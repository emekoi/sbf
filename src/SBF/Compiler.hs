module SBF.Compiler
  ( CompileOpts (..),
    Program (..),
    Pass (..),
    compile,
  )
where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Vector (Vector)
-- import qualified Data.Vector as V
import qualified SBF.Interpreter as I
import qualified SBF.Parser as P
import SBF.Parser (Op)

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

class Pass a where
  apply :: a -> Vector Op -> Vector Op

compile :: Pass p => CompileOpts -> [p] -> L.ByteString -> Program
compile o _ i =
  if interpret o
    then Interpreted (I.interpret p')
    else Compiled
  where
    p' = P.parse i
